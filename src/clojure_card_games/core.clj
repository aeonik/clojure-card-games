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

(def karbosh-deck (map vec (mapcat (partial repeat 2)
                                   (combo/cartesian-product cards suits))))

(def shuffled-deck (shuffle karbosh-deck))

(def hands (map vec (partition 8 shuffled-deck)))

(def players [:player1, :player2, :player3, :player4, :player5, :player6])

(pp/pprint (partition 8 shuffled-deck))
(pp/pprint (zipmap players hands))
(def game (zipmap players (mapv hash-map (repeat :hand) hands)))

(pp/pprint game)
(pp/pprint players)

(comment
  (def game (assoc {} :game game))
  (def game (assoc-in game [:game :player1 :team] 1))
  (def game (assoc-in game [:game :player2 :team] 2))
  (def game (assoc-in game [:game :player3 :team] 1))
  (def game (assoc-in game [:game :player4 :team] 2))
  (def game (assoc-in game [:game :player5 :team] 1))
  (def game (assoc-in game [:game :player6 :team] 2)))

(def game-with-players
  (reduce-kv (fn [data player team-number]
               (assoc-in data [:game player :team] team-number))
             {:game game}
             {:player1 1, :player2 2, :player3 1, :player4 2, :player5 1, :player6 2}))