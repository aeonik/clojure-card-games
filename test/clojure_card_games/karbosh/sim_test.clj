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

(deftest bid-strategy-comparison-test
  (let [results (sim/evaluate-bid-strategies [[:threshold :karbosh-threshold]
                                              [:probability :karbosh-probability]]
                                             [1]
                                             {:max-hands 1})]
    (is (= [:threshold :probability] (mapv :label results)))
    (is (= [1 1] (mapv :games results)))))

(deftest analysis-collection-test
  (let [state (sim/run-game 1 {:max-hands 1
                               :collect-analysis? true})
        snapshots (:sim/analysis state)]
    (is (seq snapshots))
    (is (= (count snapshots)
           (:analysis-snapshots (sim/summarize-game state))))
    (is (every? #(contains? (:analysis %) :unseen-count) snapshots))))

(deftest play-strategy-option-test
  (let [results (sim/run-games-for-seeds [1]
                                         {:max-hands 1
                                          :play-strategy :card-counting
                                          :play-config-by-team
                                          {1 bot/classic-play-config
                                           2 (assoc bot/default-play-config
                                               :lead-risk-tolerance 0.05)}})]
    (is (= 1 (count results)))
    (is (= 1 (:hands (first results))))))

(deftest play-strategy-comparison-test
  (let [results (sim/evaluate-play-strategies [[:counting :card-counting]
                                               [:probability :probability]
                                               [:hybrid :hybrid]]
                                              [1]
                                              {:max-hands 1})]
    (is (= [:counting :probability :hybrid] (mapv :label results)))
    (is (= [1 1 1] (mapv :games results)))))

(deftest play-strategy-matchup-test
  (let [result (sim/play-strategy-matchup [:old :hybrid-threshold]
                                          [:new :hybrid]
                                          [1]
                                          {:max-hands 1})]
    (is (= [:old :new] (:labels result)))
    (is (= 2 (:games result)))
    (is (= 1 (:seeds result)))
    (is (contains? result :win-rates))))
