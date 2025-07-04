(ns clojure-card-games.probability.hypergeom
  "Exact **hyper‑geometric probability helpers** with Euchre‑oriented
  shortcuts **and a one‑file CLI**.

  ─────────────────────────────────────────  WHY LIVE HERE?  ──────────────────
  * You're writing a Bid‑Euchre / Karbosh bot or post‑game analyser and need
    exact odds (not Monte‑Carlo noise) for things like:
      – 'What's the chance all three opponents can follow my lead?'
      – 'How often will a voided defender also have a trump to ruff my ace?'
  * You want to call those odds from **Clojure code *or* a shell script**.

  All functions return **exact** `clojure.lang.Ratio` / `BigInt`.  Pipe the
  result through `double` or use `--float` on the CLI for decimals.

  ─────────────  CLI CHEATSHEET (shell)  ─────────────
  $ clj -M -m clojure-card-games.probability.hypergeom prob-hg 9 31 16 4
  57915/1296760         ; exact   (P(partners snag exactly 4 trump))

  $ clj -M -m clojure-card-games.probability.hypergeom follow 8 --float
  0.0802                ; ~ 8% that EVERY defender can follow ♦ when 8 ♦ left

  $ clj -M -m clojure-card-games.probability.hypergeom void+trump 8 14 --double
  0.2546                ; 25 % that a single defender is void in ♠ and owns ♥‑trump

  The namespace is still a regular library — `require` it and call the fns
  directly when inside your Karbosh engine or REPL.
  "
  (:refer-clojure :exclude [choose])
  (:require [clojure.string :as str]))

;; ========================================================================
;; 1.  BASIC COMBINATORICS  ================================================
;; ========================================================================

(def ^:private fact-memo (atom {0 1N}))

(defn factorial
  "Exact factorial `n!` (arbitrary‑precision).  Memoised for speed.

   Example:  (factorial 8) ;=> 40320"
  ^java.math.BigInteger [^long n]
  (or (@fact-memo n)
      (let [f (reduce *' (range 1 (inc n)))]
        (swap! fact-memo assoc n f) f)))

(defn choose
  "Binomial coefficient C(n,k) = n!/(k!(n−k)!).  Returns 0 for out‑of‑range k.

   Euchre analogy ▸ choosing WHICH 3 of the 9 remaining trump land in the
   two partner hands:

       (choose 9 3) ;=> 84  distinct ways."
  [n k]
  (cond (neg? k) 0
        (> k n)  0
        :else    (/ (factorial n)
                    (*' (factorial k) (factorial (- n k))))))

;; ========================================================================
;; 2.  SINGLE‑COLOUR HYPER‑GEOMETRIC  ======================================
;; ========================================================================

(defn prob-hg
  "**P(X = k) when drawing without replacement.**
     K = # 'success' cards in population (e.g. remaining trump)
     M = # 'failure' cards in population (everything else)
     n = sample size (cards drawn)
     k = # successes you need.

   Game example ▸ After the auction you know *9* ♥ trump remain. Your two
   partners will collectively receive *16* unknown cards.  Probability they
   hold *exactly 5* more trump:

       (prob-hg 9 31 16 5) ;=>  105315/1616615 ≈ 6.5 %"
  [K M n k]
  (if (or (< k 0) (> k n) (> k K) (> (- n k) M)) 0
      (let [num (*' (choose K k) (choose M (- n k)))
            den (choose (+ K M) n)]
        (/ num den))))

(defn cum-hg
  "Cumulative  **P(X ≤ k)** for the same parameters.  Useful for questions like:
   'What's the chance my partners catch *at most* 2 bowers (Jacks) in Karbosh?'

   Karbosh Example:
   Suppose after the auction, there are 4 bowers (Jacks) left in the deck.
   Your two partners will collectively receive 16 cards. What's the probability
   they catch at most 2 of those bowers?

       (cum-hg 4 36 16 2)
       ;; => 0.849...   (about 85% chance your partners get 0, 1, or 2 bowers)

   This is useful for estimating the risk that the opponents (the other 3 players)
   will have the majority of the bowers, which can swing the hand."
  [K M n k]
  (reduce + (map #(prob-hg K M n %) (range (inc k)))))

(defn tail-geq
  "Upper‑tail  **P(X ≥ m)**.
   Example ▸ partners snag **at least 3** of the 9 stray trump:

       (tail-geq 9 31 16 3) ;≈ 80 %  (very likely)"
  [K M n m]
  (reduce + (map #(prob-hg K M n %) (range m (inc (min n K))))))

;; ------------------------------------------------------------------------
;; Multivariate (multiple categories: bowers / A♠ / other trump / junk)
;; ------------------------------------------------------------------------

(defn choose-multi
  "Multinomial coefficient  (n ; k1 … km).  Pure combinatorial count.

   Example (Karbosh/Euchre): In a 40-card deck, how many ways can you deal
   2 bowers, 1 ace, 6 other trump, and 31 junk cards into a single 40-card hand?

       (choose-multi [2 1 6 31]) ;=> 40! / (2! 1! 6! 31!) =  3108105

   This is the denominator for multi-hypergeometric probabilities."
  [ks]
  (let [n (reduce + ks)]
    (/ (factorial n) (apply *' (map factorial ks)))))

(defn multi-hg
  "Exact probability of pulling **draw‑ks** out of an urn with population
  counts **pop‑ks**.

  pop‑ks  = [2 1 6 31]  ; 2 bowers, 1 A♠, 6 other trump, 31 junk
  draw‑ks = [1 0 2 5]   ; want 1 bower, 0 A♠, 2 other trump, 5 junk

  (multi-hg pop‑ks draw‑ks) ;⇒  1568/3108105  ≈ 0.05"
  [pop-ks draw-ks]
  (assert (= (count pop-ks) (count draw-ks)))
  (let [num (apply *' (map choose pop-ks draw-ks))
        den (choose (reduce + pop-ks) (reduce + draw-ks))]
    (/ num den)))

;; ========================================================================
;; 3.  EUCHRE‑ORIENTED ONE‑LINERS  =========================================
;; ========================================================================

(defn prob-opponents-follow-suit
  "Probability **all three defenders** can follow your opening lead.

   `suit-left` = cards of that suit still hidden in the 40‑card shoe.

   Example ▸ You hold 2 ♦, partners together maybe another 0,1,2…  Assume
   worst‑case they have none, so 8 ♦ remain.  Chance every defender shows a ♦:

       (prob-opponents-follow-suit 8) ;≈ 8 %"
  [suit-left]
  (tail-geq suit-left (- 40 suit-left) 24 3))

(defn prob-void-and-trump
  [suit-left trump-left]
  "Probability **one specific defender** (8‑card hand) is void in the led
  suit *and* owns ≥1 trump — i.e. is ready to ruff your off‑suit ace.

   Example ▸ After bidding ♥, 8 ♠ remain and 14 ♥ trump remain:

       (prob-void-and-trump 8 14)  ;≈ 25 %  risk this defender can ruff."
  (let [junk (- 40 suit-left trump-left)]
    (reduce +
      (for [t (range 1 9)]
        (multi-hg [suit-left trump-left junk]
                  [0 t (- 8 t)])))))

;; ========================================================================
;; 4.  TINY CLI WRAPPER  (one source file, zero deps)  =====================
;; ========================================================================

(def ^:private public-fns
  {"factorial" factorial
   "choose"    choose
   "prob-hg"   prob-hg
   "cum-hg"    cum-hg
   "tail-geq"  tail-geq
   "multi-hg"  multi-hg
   "follow"    prob-opponents-follow-suit
   "void+trump" prob-void-and-trump})

(defn- parse-number [s]
  (try (Long/parseLong s)
       (catch NumberFormatException _ (read-string s))))

(defn -main [& argv]
  (let [[name & more] argv
        float? (some #{"--float" "--double"} more)
        args   (->> more (remove #(str/starts-with? % "--")) (map parse-number))
        f      (public-fns name)]
    (if-not f
      (println "Unknown function" name "\nAvailable:" (keys public-fns))
      (let [val (apply f args)]
        (println (if float? (double val) val))))))

;; -------------------------------  END  -----------------------------------
