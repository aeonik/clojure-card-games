(ns clojure-card-games.state-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.state :as state]))

(deftest remove-first-test
  (is (= [:a :b]     (state/remove-first :c [:a :b])))
  (is (= [:b :c]     (state/remove-first :a [:a :b :c])))
  (is (= [:a :c :b]  (state/remove-first :b [:a :b :c :b])))
  (is (= [:a :b :c]  (state/remove-first :x [:a :b :c]))))

(deftest bidding-test
  (testing "bidding is ordered and ends with the highest bidder selecting trump"
    (let [game (-> (state/init-game 7)
                   (state/apply-event {:type :bid :player :player1 :bid-type :bid :value 4})
                   (state/apply-event {:type :bid :player :player2 :bid-type :pass})
                   (state/apply-event {:type :bid :player :player3 :bid-type :bid :value 6})
                   (state/apply-event {:type :bid :player :player4 :bid-type :pass})
                   (state/apply-event {:type :bid :player :player5 :bid-type :pass})
                   (state/apply-event {:type :bid :player :player6 :bid-type :pass}))]
      (is (= :trump-selection (:phase game)))
      (is (= :player3 (:current-player game)))
      (is (= {:type :bid :player :player3 :bid-type :bid :value 6}
             (:current-bid game)))))

  (testing "out of turn bids are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"turn"
         (state/apply-event (state/init-game 7)
                            {:type :bid :player :player2 :bid-type :bid :value 4})))))

(deftest trump-selection-test
  (let [game (-> (state/init-game 7)
                 (state/apply-event {:type :bid :player :player1 :bid-type :bid :value 4})
                 (state/apply-event {:type :bid :player :player2 :bid-type :pass})
                 (state/apply-event {:type :bid :player :player3 :bid-type :pass})
                 (state/apply-event {:type :bid :player :player4 :bid-type :pass})
                 (state/apply-event {:type :bid :player :player5 :bid-type :pass})
                 (state/apply-event {:type :bid :player :player6 :bid-type :pass}))]
    (is (= :trick-playing
           (:phase (state/apply-event game
                                      {:type :trump-selection
                                       :player :player1
                                       :suit :♥}))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid trump"
         (state/apply-event game
                            {:type :trump-selection
                             :player :player1
                             :suit :stars})))))

(defn- one-trick-state []
  {:phase :trick-playing
   :history []
   :players {:player1 {:hand [[:J :♥]] :team 1}
             :player2 {:hand [[:A :♥]] :team 2}
             :player3 {:hand [[:K :♥]] :team 1}
             :player4 {:hand [[:Q :♥]] :team 2}
             :player5 {:hand [[10 :♥]] :team 1}
             :player6 {:hand [[9 :♥]] :team 2}}
   :scores {1 0 2 0}
   :current-bid {:type :bid :player :player1 :bid-type :bid :value 1}
   :current-player :player1
   :current-trick []
   :completed-tricks []
   :trump :♥
   :tricks-this-hand {1 0 2 0}
   :tricks-per-hand []
   :points-per-hand []})

(deftest play-card-test
  (testing "a complete final trick scores the hand"
    (let [game (-> (one-trick-state)
                   (state/apply-event {:type :play-card :player :player1 :card [:J :♥]})
                   (state/apply-event {:type :play-card :player :player2 :card [:A :♥]})
                   (state/apply-event {:type :play-card :player :player3 :card [:K :♥]})
                   (state/apply-event {:type :play-card :player :player4 :card [:Q :♥]})
                   (state/apply-event {:type :play-card :player :player5 :card [10 :♥]})
                   (state/apply-event {:type :play-card :player :player6 :card [9 :♥]}))]
      (is (= :hand-complete (:phase game)))
      (is (= :player1 (:current-player game)))
      (is (= {1 1 2 0} (:scores game)))
      (is (= [{:player :player1 :card [:J :♥]}
              {:player :player2 :card [:A :♥]}
              {:player :player3 :card [:K :♥]}
              {:player :player4 :card [:Q :♥]}
              {:player :player5 :card [10 :♥]}
              {:player :player6 :card [9 :♥]}]
             (first (:completed-tricks game))))))

  (testing "players must follow suit when able"
    (let [game (-> (one-trick-state)
                   (assoc-in [:players :player2 :hand] [[:A :♥] [:A :♠]])
                   (state/apply-event {:type :play-card :player :player1 :card [:J :♥]}))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Illegal card"
           (state/apply-event game
                              {:type :play-card
                               :player :player2
                               :card [:A :♠]}))))))

(deftest new-hand-test
  (let [game (state/apply-event (assoc (state/init-game 11)
                                       :phase :hand-complete
                                       :scores {1 12 2 9})
                                {:type :new-hand})]
    (is (= :bidding (:phase game)))
    (is (= {1 12 2 9} (:scores game)))
    (is (= 1 (:hand-index game)))
    (is (= :player2 (:dealer game)))))
