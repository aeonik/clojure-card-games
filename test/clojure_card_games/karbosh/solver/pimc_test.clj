(ns clojure-card-games.karbosh.solver.pimc-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.solver.pimc :as pimc]))

(defn state
  [{:keys [hands teams active-players current-player current-trick trump tricks]}]
  {:players (into {}
                  (map (fn [[player hand]]
                         [player {:hand hand
                                  :team (get teams player)}])
                       hands))
   :active-players active-players
   :current-player current-player
   :current-trick (or current-trick [])
   :trump trump
   :tricks-this-hand (or tricks {1 0 2 0})})

(def two-player-options
  {:players [:player1 :player2]
   :hand-size 1})

(deftest sample-state-test
  (testing "current trick cards are removed from sampled hidden hands"
    (let [s (state {:hands {:player1 []
                            :player2 []}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player2
                    :current-trick [{:player :player1 :card [:A :♥]}]
                    :trump :♠})
          world (pimc/sample-state s
                                   (assoc two-player-options
                                          :seed 1
                                          :hand-sizes {:player1 0
                                                       :player2 1}
                                          :deck [[:A :♥] [:K :♥]]))]
      (is (= [] (get-in world [:players :player1 :hand])))
      (is (= [[:K :♥]] (get-in world [:players :player2 :hand]))))))

(deftest evaluate-contract-test
  (testing "forced making bid has make rate one and positive EV"
    (let [s (state {:hands {:player1 [[:A :♥]]
                            :player2 []}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player1
                    :trump :♠})
          result (pimc/evaluate-contract
                   s
                   {:type :bid :player :player1 :bid-type :bid :value 1}
                   (assoc two-player-options
                          :seeds [1 2 3]
                          :deck [[:A :♥] [:K :♥]]))]
      (is (= 3 (:samples result)))
      (is (= 1.0 (:make-rate result)))
      (is (= 1.0 (:ev result)))
      (is (= [1 1 1] (mapv :value (:outcomes result))))))

  (testing "forced failed bid has downside rate one and negative EV"
    (let [s (state {:hands {:player1 [[:K :♥]]
                            :player2 []}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player1
                    :trump :♠})
          result (pimc/evaluate-contract
                   s
                   {:type :bid :player :player1 :bid-type :bid :value 1}
                   (assoc two-player-options
                          :seeds [1 2 3]
                          :deck [[:A :♥] [:K :♥]]))]
      (is (= 0.0 (:make-rate result)))
      (is (= 1.0 (:downside-rate result)))
      (is (= -2.0 (:ev result)))
      (is (= [-2 -2 -2] (mapv :value (:outcomes result)))))))
