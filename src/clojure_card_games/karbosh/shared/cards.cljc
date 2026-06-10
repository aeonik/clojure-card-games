(ns clojure-card-games.karbosh.shared.cards
  "Karbosh card configuration: a double deck of the six ranks from 9 to ace.

  This is the canonical deck definition for Karbosh. The generic card model
  lives in `clojure-card-games.cards` and `clojure-card-games.deck` so other
  games can define their own deck specs."
  (:require [clojure-card-games.cards :as generic-cards]
            [clojure-card-games.deck :as generic-deck]))

(def ranks [9 10 :J :Q :K :A])
(def suits generic-cards/suits)

(def rank->str (select-keys generic-cards/rank->str ranks))
(def suit->str generic-cards/suit->str)

(def str->rank
  (into {} (map (fn [rank] [(rank->str rank) rank])) ranks))
(def str->suit generic-cards/str->suit)

(def char->rank {\a :A \k :K \q :Q \j :J \0 10 \1 10 \9 9})
(def char->suit generic-cards/char->suit)

(def card->str generic-cards/card->str)

(def parse-tables
  {:str->rank str->rank
   :char->rank char->rank
   :char->suit char->suit})

(defn parse-card [s]
  (generic-cards/parse-card s parse-tables))

(def deck-spec
  "Two physical copies of every rank/suit combination, 48 cards."
  {:ranks ranks :suits suits :copies 2})

(defn deck []
  (generic-deck/build-deck deck-spec))

#?(:clj
   (def shuffle-deck generic-deck/shuffle-deck))

(def hand-size 8)

(defn deal [cards]
  (generic-deck/deal-hands cards hand-size))
