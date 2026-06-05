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
