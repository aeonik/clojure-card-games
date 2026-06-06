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
