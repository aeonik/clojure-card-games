(ns clojure-card-games.karbosh.analysis-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]))

(deftest effective-suit-counts-test
  (testing "left bower moves into trump for hidden-card suit counts"
    (is (= {:♥ 14 :♠ 12 :♦ 10 :♣ 12}
           (analysis/effective-suit-counts :♥ (cards/deck))))))

(deftest variable-hand-size-hypergeom-test
  (testing "all labeled hands can receive a success with uneven hand sizes"
    (is (= 1 (analysis/prob-labeled-hand-sizes-min-success
               3 0 [1 2] 1)))
    (is (= 0 (analysis/prob-labeled-hand-sizes-min-success
               1 2 [1 2] 1))))

  (testing "any-success probability handles empty and impossible samples"
    (is (= 0 (analysis/probability-of-any-success 0 3 [1 1])))
    (is (= 0 (analysis/probability-of-any-success 1 1 [2])))
    (is (= 1 (analysis/probability-of-any-success 1 1 [1])))))

(deftest successful-hand-count-distribution-test
  (testing "counts how many labeled hands contain a success"
    (is (= {2 1}
           (analysis/successful-hand-count-distribution 2 0 [1 1] 1)))
    (is (= {1 1}
           (analysis/successful-hand-count-distribution 1 1 [1 1] 1)))
    (is (= 1
           (analysis/probability-at-least-successful-hands 2 0 [1 1] 2)))
    (is (= 0
           (analysis/probability-at-least-successful-hands 1 1 [1 1] 2)))))

(deftest player-analysis-test
  (testing "bidding analysis reports every candidate trump"
    (let [state (game/init-game 1)
          odds (analysis/player-analysis state :player1)]
      (is (= :bidding (:phase odds)))
      (is (= 8 (:hand-count odds)))
      (is (= 40 (:unseen-count odds)))
      (is (= (set cards/suits) (set (keys (:candidate-trumps odds)))))
      (is (nil? (:trump-analysis odds)))))

  (testing "trick analysis uses seen cards and current trump"
    (let [game {:phase :trick-playing
                :trump :♠
                :active-players game/players
                :players {:player1 {:team 1
                                    :hand [[:A :♠]]}
                          :player2 {:team 2
                                    :hand [[:K :♥]]}
                          :player3 {:team 1
                                    :hand []}
                          :player4 {:team 2
                                    :hand []}
                          :player5 {:team 1
                                    :hand []}
                          :player6 {:team 2
                                    :hand []}}
                :completed-tricks [[{:player :player2 :card [:J :♠]}
                                    {:player :player3 :card [:J :♠]}]]
                :current-trick []}
          odds (analysis/player-analysis game :player1)
          trump-odds (:trump-analysis odds)
          card-odds (first (:hand trump-odds))]
      (is (= 3 (:seen-count odds)))
      (is (= 45 (:unseen-count odds)))
      (is (= :♠ (:trump trump-odds)))
      (is (= 0 (get (:unseen-card-counts odds) [:J :♠] 0)))
      (is (= [:A :♠] (:card card-odds)))
      (is (number? (:prob-pending-opponent-has-higher-card card-odds)))))

  (testing "off-suit winners use ruff odds instead of treating every trump as live"
    (let [hidden-hand (vec (repeat 8 [9 :♣]))
          game {:phase :trick-playing
                :trump :♠
                :active-players game/players
                :players {:player1 {:team 1
                                    :hand [[:A :♥]]}
                          :player2 {:team 2
                                    :hand hidden-hand}
                          :player3 {:team 1
                                    :hand hidden-hand}
                          :player4 {:team 2
                                    :hand hidden-hand}
                          :player5 {:team 1
                                    :hand hidden-hand}
                          :player6 {:team 2
                                    :hand hidden-hand}}
                :current-trick []}
          card-odds (-> (analysis/player-analysis game :player1)
                        :trump-analysis
                        :hand
                        first)]
      (is (pos? (:higher-trump-unseen card-odds)))
      (is (< (:prob-pending-opponent-can-beat-card card-odds)
             (:prob-pending-opponent-has-higher-card card-odds))))))

(def ybhybh-control-collision-game
  {:phase :trick-playing
   :trump :♣
   :active-players game/players
   :players {:player1 {:team 1
                       :hand [[:Q :♦] [:J :♦] [10 :♣] [9 :♥] [:K :♣]]}
             :player2 {:team 2
                       :hand [[10 :♦] [:A :♦] [:J :♥] [:A :♦] [:Q :♥]]}
             :player3 {:team 1
                       :hand [[:K :♥] [:K :♦] [:K :♦] [:J :♠] [10 :♦]]}
             :player4 {:team 2
                       :hand [[:Q :♥] [:J :♥] [:Q :♦] [:K :♥] [:A :♥]]}
             :player5 {:team 1
                       :hand [[:K :♠] [9 :♥] [:K :♠] [10 :♠] [10 :♥]]}
             :player6 {:team 2
                       :hand [[9 :♦] [:J :♦] [:A :♥] [10 :♥] [:Q :♠]]}}
   :completed-tricks [[{:player :player1 :card [:J :♣]}
                       {:player :player2 :card [9 :♣]}
                       {:player :player3 :card [9 :♣]}
                       {:player :player4 :card [10 :♣]}
                       {:player :player5 :card [:A :♣]}
                       {:player :player6 :card [:A :♣]}]
                      [{:player :player1 :card [:J :♣]}
                       {:player :player2 :card [:Q :♣]}
                       {:player :player3 :card [:K :♣]}
                       {:player :player4 :card [:Q :♣]}
                       {:player :player5 :card [9 :♦]}
                       {:player :player6 :card [:J :♠]}]
                      [{:player :player1 :card [:A :♠]}
                       {:player :player2 :card [:A :♠]}
                       {:player :player3 :card [:Q :♠]}
                       {:player :player4 :card [9 :♠]}
                       {:player :player5 :card [9 :♠]}
                       {:player :player6 :card [10 :♠]}]]
   :current-trick []})

(deftest partner-control-burn-analysis-test
  (testing "known trump voids condition forced partner-control burn exactly"
    (let [analysis (analysis/card-defeat-analysis ybhybh-control-collision-game
                                                  :player1
                                                  [10 :♣])]
      (is (= {:player5 #{:♣}}
             (select-keys (analysis/known-voids ybhybh-control-collision-game)
                          [:player5])))
      (is (= 1/4
             (get-in analysis
                     [:prob-pending-partner-forced-higher-follow :player3])))
      (is (= 0
             (get-in analysis
                     [:prob-pending-partner-forced-higher-follow :player5])))
      (is (= 1/4
             (:expected-pending-partner-control-burn analysis))))))
