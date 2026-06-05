(ns clojure-card-games.karbosh.sim-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.sim :as sim]))

(deftest max-hands-guard-test
  (let [state (sim/run-game 1 {:max-hands 1})]
    (is (= :max-hands (:sim/stop-reason state)))
    (is (= 1 (count (:hand-history state))))))

(deftest min-score-guard-test
  (let [state (sim/run-game 1 {:min-score 0})]
    (is (= :min-score (:sim/stop-reason state)))
    (is (= 0 (count (:hand-history state))))))

(deftest explicit-seed-run-test
  (let [results (sim/run-games-for-seeds [2 5] {:max-hands 1})]
    (is (= [2 5] (mapv :seed results)))
    (is (= [1 1] (mapv :hands results)))))

(deftest bid-config-comparison-test
  (let [configs [[:default bot/default-bid-config]
                 [:no-sixes (assoc bot/default-bid-config
                              :bid-6-strength 999999)]]
        results (sim/evaluate-bid-configs configs [1 2] {:max-hands 1})]
    (is (= [:default :no-sixes] (mapv :label results)))
    (is (= [2 2] (mapv :games results)))))
