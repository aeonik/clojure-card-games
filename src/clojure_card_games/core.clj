(ns clojure-card-games.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def cards (vector 9 10 :J :Q :K :A))
(def suits (vector :♥ :♠ :♦ :♣))

(print cards)
(print suits)

(pp/pprint (map vec (combo/cartesian-product cards suits)))
(def karbosh-deck (map vec (mapcat (partial repeat 2)
                                   (combo/cartesian-product cards suits))))
(pp/pprint karbosh-deck)

(def shuffled-deck (shuffle karbosh-deck))
(pp/pprint shuffled-deck)

(def hands (map vec (partition 8 shuffled-deck)))
(pp/pprint hands)

(def players [:player1, :player2, :player3, :player4, :player5, :player6])
(pp/pprint players)

(pp/pprint (partition 8 shuffled-deck))
(pp/pprint (zipmap players hands))
(def game (zipmap players (mapv hash-map (repeat :hand) hands)))

(pp/pprint game)
(pp/pprint players)

(def game (assoc {} :game game))
(def game (assoc-in game [:game :player1 :team] 1))
(def game (assoc-in game [:game :player2 :team] 2))
(def game (assoc-in game [:game :player3 :team] 1))
(def game (assoc-in game [:game :player4 :team] 2))
(def game (assoc-in game [:game :player5 :team] 1))
(def game (assoc-in game [:game :player6 :team] 2))
(pp/pprint game)

(def game-with-players
  (reduce-kv (fn [data player team-no]
               (assoc-in data [:game player :team] team-no))
             {:game game}
             {:player1 1, :player2 2, :player3 3, :player4 4, :player5 5, :player6 6}))