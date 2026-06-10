(ns clojure-card-games.cards
  "Game-agnostic playing-card model.

  A card is a logical `[rank suit]` tuple. Games pick the rank subset they
  use (Karbosh uses 9 through ace, future games may use the full 2 through
  ace range) and how many physical copies of each card enter the deck; see
  `clojure-card-games.deck`."
  (:require [clojure.string :as str]))

(def standard-ranks
  "All standard ranks in ascending strength order (ace high)."
  [2 3 4 5 6 7 8 9 10 :J :Q :K :A])

(def suits [:♥ :♠ :♦ :♣])

(def rank->int (zipmap standard-ranks (range)))
(def suit->int (zipmap suits (range)))

(def rank->str
  {2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10"
   :J "J" :Q "Q" :K "K" :A "A"})

(def suit->str {:♥ "♥" :♠ "♠" :♦ "♦" :♣ "♣"})

(def str->rank
  {"2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "10" 10
   "J" :J "Q" :Q "K" :K "A" :A})

(def str->suit
  {"♥" :♥ "♠" :♠ "♦" :♦ "♣" :♣
   "h" :♥ "s" :♠ "d" :♦ "c" :♣})

(def char->rank
  "Single-character rank input. `\\0` and `\\1` both mean 10 because no card
  game uses a rank of 1; the ace is always `:A`."
  {\a :A \k :K \q :Q \j :J \0 10 \1 10
   \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})

(def char->suit {\h :♥ \s :♠ \d :♦ \c :♣})

(defn card->str [[rank suit]]
  (str (rank->str rank) (suit->str suit)))

(defn parse-card
  "Parse text like \"Qh\", \"10♦\", or \"a♠\" into a `[rank suit]` card.

  Games with restricted rank sets pass their own lookup `tables`
  (`:str->rank`, `:char->rank`, `:char->suit`) so out-of-game ranks fail to
  parse instead of producing impossible cards."
  ([s] (parse-card s nil))
  ([s tables]
   (let [rank-table (or (:str->rank tables) str->rank)
         rank-chars (or (:char->rank tables) char->rank)
         suit-chars (or (:char->suit tables) char->suit)
         s (str/lower-case (str/trim s))
         suit (or (suit-chars (last s))
                  (str->suit (str (last s))))
         rank-text (subs s 0 (max 0 (dec (count s))))
         rank (or (rank-table (str/upper-case rank-text))
                  (rank-chars (first rank-text)))]
     (when (and rank suit)
       [rank suit]))))

;; ---------------------------------------------------------------------------
;; Display helpers
;; ---------------------------------------------------------------------------

(def suit->base {:♠ 0x1F0A0, :♥ 0x1F0B0, :♦ 0x1F0C0, :♣ 0x1F0D0})

(def rank->offset
  "Offsets into the Unicode playing-card block. Code point 12 in each suit is
  the knight, which no standard deck uses, so queen and king sit at 13/14."
  {:A 1, 2 2, 3 3, 4 4, 5 5, 6 6, 7 7, 8 8, 9 9, 10 10, :J 11, :Q 13, :K 14})

(def suit->ansi {:♥ 31, :♦ 31, :♠ 30, :♣ 30})

#?(:clj
   (defn unicode [[rank suit]]
     (let [base (suit->base suit)
           offset (rank->offset rank)]
       (assert base (str "Bad suit " suit))
       (assert offset (str "Bad rank " rank))
       (String. (Character/toChars (+ base offset))))))

(defn sort-hand
  "Sort by suit (♥ < ♠ < ♦ < ♣) then rank ascending."
  [hand]
  (sort-by (juxt (comp suit->int second)
                 (comp rank->int first))
           hand))
