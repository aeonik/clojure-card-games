(ns clojure-card-games.test-constants)

;; Test Seeds
(def KARBOSH-HAND-SEED -793572874536499429)

;; Test Scenarios
(def test-scenarios
  {:karbosh-hand
   {:seed KARBOSH-HAND-SEED
    :description "A hand that should result in a Karbosh bid"
    :expected-outcome "Player should be able to make Karbosh bid"}})

;; Usage:
;; clj -M -m clojure-card-games.cli -793572874536499429