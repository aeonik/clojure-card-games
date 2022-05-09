(ns clojure-card-games.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def cards 
  "Karbosh cards."
  (vector 9 10 :J :Q :K :A))
(def suits 
  "Karbosh suits."
  (vector :♥ :♠ :♦ :♣))

(def karbosh-deck 
  "Create karbosh deck. Two decks from 9 to Ace of each suit."
  (map vec (mapcat (partial repeat 2)
                                   (combo/cartesian-product cards suits))))

(def shuffled-deck 
  "Shuffle the deck."
  (shuffle karbosh-deck))

(def hands 
  "Break the deck into hands with 8 cards each."
  (map vec (partition 8 shuffled-deck)))

(def players [:player1, :player2, :player3, :player4, :player5, :player6])

(pp/pprint (partition 8 shuffled-deck))
(pp/pprint (zipmap players hands))
(def game (zipmap players (mapv hash-map (repeat :hand) hands)))

(pp/pprint game)
(pp/pprint players)

(comment
  "This is the old way of doing it. Not idiomatic in Clojure.
   From Sean Cornfield: The first bit of advice I'd offer is: don't treat def like an assignment would be in other languages. 
   First off, def always introduces a global (top-level) Var -- you don't show much of your code but you should never use def inside a function.
   We generally think of def as introducing a single global constant.
   Second, for repeated operations on a single value where we want to accumulate the result, take a look at -> : "
  (def game (assoc {} :game game))
  (def game (assoc-in game [:game :player1 :team] 1))
  (def game (assoc-in game [:game :player2 :team] 2))
  (def game (assoc-in game [:game :player3 :team] 1))
  (def game (assoc-in game [:game :player4 :team] 2))
  (def game (assoc-in game [:game :player5 :team] 1))
  (def game (assoc-in game [:game :player6 :team] 2)))

(comment 
  "Sean Cornfield's way of doing it."
  (def game-with-players
           (-> {:game game}
               (assoc-in [:game :player1 :team] 1)
               (assoc-in [:game :player2 :team] 2)
               (assoc-in [:game :player3 :team] 3)
               ...etc...)))

(def game-with-players
  "Thanks to Sean Cornfield for this example:
   A more advanced approach would be to represent 
   the changes you want to make as a data structure 
   -- in this case a hash map from player keys to team numbers 
   -- and then reduce over that to make the changes you need:"
  (reduce-kv (fn [data player team-number]
               (assoc-in data [:game player :team] team-number))
             {:game game}
             {:player1 1, :player2 2, :player3 1, :player4 2, :player5 1, :player6 2}))