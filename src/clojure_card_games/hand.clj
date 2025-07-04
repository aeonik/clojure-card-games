(ns clojure-card-games.hand
  (:require [clojure-card-games.rules :as rules]))

;; Pure hand evaluation and sorting utilities

(defn sort-hand [hand trump]
  ;; Sort hand by card value (highest first), using rules/card-value
  (sort-by #(rules/card-value % trump nil) > hand))

(defn evaluate-hand-strength [hand trump]
  ;; Evaluate hand strength for bidding: sum of card values
  (reduce + (map #(rules/card-value % trump nil) hand)))