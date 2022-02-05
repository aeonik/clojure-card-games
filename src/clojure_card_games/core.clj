(ns clojure-card-games.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def cards (list 9 10 :J :Q :K :A))
(def suits (list :♥ :♠ :♦ :♣))

(print cards)
(print suits)

(print (combo/cartesian-product cards suits))
(def karbosh-deck (mapcat (partial repeat 2)
                          (combo/cartesian-product cards suits)))
(print karbosh-deck)

(def shuffled-deck (shuffle karbosh-deck))
(print shuffled-deck)

(run! println (partition 6 shuffled-deck))

(print (map :K shuffled-deck))