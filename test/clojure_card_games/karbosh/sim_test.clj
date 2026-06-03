(ns clojure-card-games.karbosh.sim-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.sim :as sim]))

(deftest max-hands-guard-test
  (let [state (sim/run-game 1 {:max-hands 1})]
    (is (= :max-hands (:sim/stop-reason state)))
    (is (= 1 (count (:hand-history state))))))

(deftest min-score-guard-test
  (let [state (sim/run-game 1 {:min-score 0})]
    (is (= :min-score (:sim/stop-reason state)))
    (is (= 0 (count (:hand-history state))))))
