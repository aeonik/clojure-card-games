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

(deftest prepare-karbosh-world-test
  (testing "discards from the caller and donates from partners before play"
    (let [s (state {:hands {:player1 [[10 :♠] [:J :♣] [:J :♥]]
                            :player2 [[:Q :♥]]
                            :player3 [[:A :♠]]
                            :player4 [[:K :♥]]
                            :player5 [[:K :♠]]
                            :player6 [[:A :♣]]}
                    :teams {:player1 1
                            :player2 2
                            :player3 1
                            :player4 2
                            :player5 1
                            :player6 2}
                    :active-players [:player1 :player2 :player3
                                     :player4 :player5 :player6]
                    :current-player :player1
                    :trump :♥})
          world (pimc/prepare-karbosh-world
                  s
                  {:type :bid :player :player1 :bid-type :karbosh}
                  :♥)]
      (is (= [[:J :♥] [:A :♠] [:K :♠]]
             (get-in world [:players :player1 :hand])))
      (is (= [] (get-in world [:players :player3 :hand])))
      (is (= [] (get-in world [:players :player5 :hand])))
      (is (= [:player1 :player2 :player4 :player6]
             (:active-players world)))
      (is (= :player1 (:current-player world))))))

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

(deftest evaluate-contract-make-test
  (testing "numeric make evaluation reports make probability"
    (let [s (state {:hands {:player1 [[:A :♥]]
                            :player2 []}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player1
                    :trump :♠})
          result (pimc/evaluate-contract-make
                   s
                   {:type :bid :player :player1 :bid-type :bid :value 1}
                   (assoc two-player-options
                          :seeds [1 2 3]
                          :deck [[:A :♥] [:K :♥]]))]
      (is (= 3 (:samples result)))
      (is (= 1.0 (:make-rate result)))
      (is (= [true true true] (mapv :made? (:outcomes result))))))

  (testing "karbosh make evaluation uses binary all-tricks outcome"
    (let [s (state {:hands {:player1 [[:K :♥]]
                            :player2 []}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player1
                    :trump :♠})
          result (pimc/evaluate-contract-make
                   s
                   {:type :bid :player :player1 :bid-type :karbosh}
                   (assoc two-player-options
                          :seeds [1 2 3]
                          :deck [[:A :♥] [:K :♥]]))]
      (is (= 0.0 (:make-rate result)))
      (is (= [false false false] (mapv :made? (:outcomes result)))))))
