(ns clojure-card-games.probability.probability
  "Monte‑Carlo + analytic helpers for Karbosh probability questions.

  *Pure, side‑effect‑free* — nothing here touches the REPL, I/O or your
  existing TUI.  Require the namespace from `clojure-card-games.runner!`
  or wherever you like and call the fns directly.

  Example (quick sanity check):

      (require '[clojure-card-games.probability :as prob])
      (def hero [[:J :♠] [:J :♠] [:A :♠] [:K :♠] [:Q :♠] [:A :♦] [10 :♥] [9 :♥]])
      (prob/karbosh-win-empirical hero 100_000)  ;; ⇒ ≃ 0.57

  Adjust the predicates in `killer-hand?` if your house rules differ."
  (:require [clojure-card-games.karbosh.shared.cards :as karbosh-cards]))

;; ----------------------------------------------------------------------------
;; 1. Helper predicates -------------------------------------------------------
;; ----------------------------------------------------------------------------

(def left-bower? (partial = [:J :♣]))

(defn- remove-first [x coll]
  (let [[before after] (split-with #(not= x %) coll)]
    (when-not (seq after)
      (throw (ex-info "Card is not available in the deck" {:card x})))
    (vec (concat before (rest after)))))

(defn- shuffle-with [coll ^java.util.Random rng]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al rng)
    (vec al)))

(defn trump?
  "Return true iff the card is trump under standard double‑deck Karbosh rules.
  Trump is always ♠ plus BOTH J♣ left‑bowers."
  [[rank suit]]
  (or (= suit :♠) (left-bower? [rank suit])))

;; ----------------------------------------------------------------------------
;; 2. Killer‑hand predicate  --------------------------------------------------
;; ----------------------------------------------------------------------------

(defn killer-hand?
  "Does `hand` satisfy the loss conditions against hero?
   `mates` is a seq of the two partner hands so we can test the
   (both left‑bowers with partners) edge‑case."
  [hand mates]
  (let [t       (filter trump? hand)
        t#      (count t)
        has-LB? (some left-bower? hand)
        has-A♠? (some #{[:A :♠]} hand)
        both-LB-in-mates? (= 2 (count (filter left-bower? (apply concat mates))))]
    (or                                                   ;; 1. bower + ≥2 extra trump
     (and (>= t# 3) has-LB?)                           ;; --------------------------
     ;; 2. both bowers are safe with mates BUT enemy got ≥5 other trump + A♠
     (and both-LB-in-mates?
          (>= (- t# (if has-LB? 1 0)) 5)  ;; ignore LB if he somehow stole one
          has-A♠?))))

;; ----------------------------------------------------------------------------
;; 3. Single‑deal simulation --------------------------------------------------
;; ----------------------------------------------------------------------------

(defn hero-wins-once?
  "Return true if HERO wins the hand in one random deal. Takes:
   * `hero` — 8‑card vector that HERO already has.
   * optional `rng`  — java.util.Random instance for reproducibility."
  ([hero] (hero-wins-once? hero (java.util.Random.)))
  ([hero ^java.util.Random rng]
   (let [shoe  (shuffle-with (reduce #(remove-first %2 %1)
                                     (karbosh-cards/deck)
                                     hero)
                             rng)
         hands (partition 8 shoe)
         mates (take 2 hands)
         opps  (drop 2 hands)
         lose? (some #(killer-hand? % mates) opps)]
     (not lose?))))

;; ----------------------------------------------------------------------------
;; 4. Monte‑Carlo wrapper -----------------------------------------------------
;; ----------------------------------------------------------------------------

(defn karbosh-win-empirical
  "Estimate HERO's Karbosh success probability via Monte‑Carlo.
   * `hero-hand` — 8‑card vector (e.g. from the current game state)
   * `n` — number of trials (default 100 000)
   * `seed` — optional Long for reproducibility.

   Returns the proportion of trials HERO wins."
  ([hero-hand] (karbosh-win-empirical hero-hand 100000 nil))
  ([hero-hand n] (karbosh-win-empirical hero-hand n nil))
  ([hero-hand n seed]
   (let [rng   (java.util.Random. (or seed (System/nanoTime)))
         wins  (loop [i n, w 0]
                 (if (zero? i)
                   w
                   (recur (dec i)
                          (if (hero-wins-once? hero-hand rng) (inc w) w))))]
     (/ wins (double n)))))

;; ----------------------------------------------------------------------------
;; 5. Analytic stub (hyper‑geo) ----------------------------------------------
;; ----------------------------------------------------------------------------
;; Exact helpers live in clojure-card-games.probability.hypergeom.
