(ns clojure-card-games.karbosh.shared.cards
  (:require [clojure.string :as str]))

(def ranks [9 10 :J :Q :K :A])
(def suits [:♥ :♠ :♦ :♣])

(def rank->str {9 "9" 10 "10" :J "J" :Q "Q" :K "K" :A "A"})
(def suit->str {:♥ "♥" :♠ "♠" :♦ "♦" :♣ "♣"})

(def str->rank {"9" 9 "10" 10 "J" :J "Q" :Q "K" :K "A" :A})
(def str->suit {"♥" :♥ "♠" :♠ "♦" :♦ "♣" :♣
                "h" :♥ "s" :♠ "d" :♦ "c" :♣})

(def char->rank {\a :A \k :K \q :Q \j :J \0 10 \1 10 \9 9})
(def char->suit {\h :♥ \s :♠ \d :♦ \c :♣})

(defn card->str [[rank suit]]
  (str (rank->str rank) (suit->str suit)))

(defn parse-card [s]
  (let [s (str/lower-case (str/trim s))
        suit (char->suit (last s))
        rank-text (subs s 0 (max 0 (dec (count s))))
        rank (or (str->rank (str/upper-case rank-text))
                 (char->rank (first rank-text)))]
    (when (and rank suit)
      [rank suit])))

(defn deck []
  (vec (for [rank ranks
             suit suits
             _ (range 2)]
         [rank suit])))

#?(:clj
   (defn shuffle-deck
     ([cards] (shuffle-deck cards nil))
     ([cards seed]
      (let [al (java.util.ArrayList. cards)
            rng (if seed (java.util.Random. seed) (java.util.Random.))]
        (java.util.Collections/shuffle al rng)
        (vec al)))))

(defn deal [cards]
  (mapv vec (partition 8 cards)))
