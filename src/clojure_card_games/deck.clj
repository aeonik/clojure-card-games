(ns clojure-card-games.deck
  (:require [clojure-card-games.cards :as cards]))

(defn karbosh-deck []
  ;; Two of each card, all ranks and suits
  (vec (for [rank cards/ranks
             suit cards/suits
             _ (range 2)]
         [rank suit])))

(defn shuffle-deck
  ([deck] (shuffle-deck deck nil))
  ([deck seed]
   (let [al (java.util.ArrayList. deck)
         rng (if seed (java.util.Random. seed) (java.util.Random.))]
     (java.util.Collections/shuffle al rng)
     (vec al))))

(defn deal-hands [deck]
  ;; 6 hands of 8 cards each
  (mapv vec (partition 8 deck)))
