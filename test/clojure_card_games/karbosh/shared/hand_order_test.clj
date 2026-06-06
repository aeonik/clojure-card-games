(ns clojure-card-games.karbosh.shared.hand-order-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.shared.hand-order :as hand-order]))

(deftest reconcile-preserves-duplicate-cards-test
  (let [ace-heart [:A :♥]
        ace-diamond [:A :♦]
        hand [ace-heart ace-heart ace-diamond]
        order {:hand-index 4
               :cards [ace-heart ace-diamond]}]
    (is (= {:hand-index 4
            :cards [ace-heart ace-diamond ace-heart]}
           (hand-order/reconcile order hand 4)))))

(deftest reconcile-drops-stale-duplicate-cards-test
  (let [ace-heart [:A :♥]
        ace-diamond [:A :♦]
        hand [ace-heart ace-diamond]
        order {:hand-index 4
               :cards [ace-heart ace-heart ace-diamond]}]
    (is (= {:hand-index 4
            :cards [ace-heart ace-diamond]}
           (hand-order/reconcile order hand 4)))))

(deftest visible-hand-removes-only-one-pending-copy-test
  (let [ace-heart [:A :♥]
        king-spade [:K :♠]]
    (is (= [ace-heart king-spade]
           (hand-order/visible-hand [ace-heart ace-heart king-spade]
                                    ace-heart)))))

(deftest move-card-to-preserves-duplicate-cards-test
  (let [ace-heart [:A :♥]
        king-spade [:K :♠]
        ace-diamond [:A :♦]]
    (is (= [king-spade ace-heart ace-heart ace-diamond]
           (hand-order/move-card-to [ace-heart king-spade ace-heart ace-diamond]
                                    ace-heart
                                    2)))))

(deftest move-index-to-reorders-duplicate-cards-test
  (let [ace-heart [:A :♥]
        king-spade [:K :♠]
        queen-club [:Q :♣]]
    (is (= {:cards [ace-heart king-spade ace-heart queen-club]
            :index 2}
           (hand-order/move-index-to [ace-heart ace-heart king-spade queen-club]
                                     0
                                     2
                                     true)))
    (is (= {:cards [ace-heart queen-club ace-heart king-spade]
            :index 1}
           (hand-order/move-index-to [ace-heart ace-heart king-spade queen-club]
                                     3
                                     0
                                     true)))))

(deftest sorted-hand-uses-trump-and-alternates-suit-colors-test
  (let [hand [[:A :♥] [:K :♥] [:J :♥] [:J :♦]
              [:A :♦] [:K :♦]
              [:A :♠] [:K :♠]
              [:A :♣]]]
    (is (= [[:J :♥] [:J :♦] [:A :♥] [:K :♥]
            [:A :♠] [:K :♠]
            [:A :♦] [:K :♦]
            [:A :♣]]
           (hand-order/sorted-hand hand :♥)))))

(deftest sorted-hand-evaluates-potential-trump-before-trump-is-known-test
  (let [hand [[:J :♥] [:J :♦] [:A :♦]
              [:A :♠] [:K :♠]
              [:K :♣]]]
    (is (= [[:J :♦] [:J :♥] [:A :♦]
            [:A :♠] [:K :♠]
            [:K :♣]]
           (hand-order/sorted-hand hand nil)))))

(deftest sorted-hand-repeats-potential-trump-selection-test
  (let [hand [[:J :♥] [:J :♦] [:A :♥] [:K :♥]
              [:A :♠] [:K :♠]
              [:A :♦]
              [:K :♣]]]
    (is (= [[:J :♥] [:J :♦] [:A :♥] [:K :♥]
            [:A :♠] [:K :♠]
            [:A :♦]
            [:K :♣]]
           (hand-order/sorted-hand hand nil)))))
