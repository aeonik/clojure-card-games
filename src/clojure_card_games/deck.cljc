(ns clojure-card-games.deck
  "Game-agnostic deck construction, shuffling, and dealing."
  (:require [clojure-card-games.cards :as cards]))

(defn build-deck
  "Build a deck from a spec map.

  `:ranks`  ranks to include (default all standard ranks)
  `:suits`  suits to include (default all four)
  `:copies` physical copies of each logical card (default 1; Karbosh uses 2)"
  ([] (build-deck nil))
  ([{:keys [ranks suits copies]
     :or {ranks cards/standard-ranks
          suits cards/suits
          copies 1}}]
   (vec (for [rank ranks
              suit suits
              _ (range copies)]
          [rank suit]))))

#?(:clj
   (defn shuffle-deck
     "Shuffle `deck`, reproducibly when `seed` is given."
     ([deck] (shuffle-deck deck nil))
     ([deck seed]
      (let [al (java.util.ArrayList. ^java.util.Collection deck)
            rng (if seed (java.util.Random. seed) (java.util.Random.))]
        (java.util.Collections/shuffle al rng)
        (vec al)))))

(defn deal-hands
  "Deal consecutive hands of `hand-size` cards from `deck`."
  [deck hand-size]
  (mapv vec (partition hand-size deck)))
