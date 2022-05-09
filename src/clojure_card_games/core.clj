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

(defn shuffled-deck [karbosh-deck]
  "Shuffle the deck."
  (shuffle karbosh-deck))

(defn shuffled-hands [shuffled-deck]
  "Break the deck into hands with 8 cards each."
  (map vec (partition 8 shuffled-deck)))

(def players [:player1, :player2, :player3, :player4, :player5, :player6])

(pp/pprint (partition 8 shuffled-deck))
(pp/pprint (zipmap players shuffled-hands))
(def game (zipmap players (mapv hash-map (repeat :hand) shuffled-hands)))

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
  "Sean Cornfield's slightly better way of doing it."
  (def game-with-players
    (-> {:game game}
        (assoc-in [:game :player1 :team] 1)
        (assoc-in [:game :player2 :team] 2)
        (assoc-in [:game :player3 :team] 1)
        (assoc-in [:game :player4 :team] 2)
        (assoc-in [:game :player5 :team] 1)
        (assoc-in [:game :player6 :team] 2))))

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

(defn set-trump [game-with-players trump-suit]
  "Add random trump suit to the game using rand-nth and assoc-in"
  (assoc-in game-with-players [:game :trump] trump-suit))

(defn set-dealer [game-with-players player]
  "Add player as dealer suit to the game using rand-nth and assoc-in"
  (assoc-in game-with-players [:game player :dealer] true))

(defn start-game [game-with-players]
  (-> game-with-players
      (set-trump :♠)
      (set-dealer :player1)))

(defn create-trick [game]
  "Create a trick, deal one card from each player's hand to the trick"
  (comment
  "This is a naive way of doing it. It gets the first card of each hand
   Probably want to use Dissoc instead though."
    (map first (map :hand (vals (:game game-with-players))))))
