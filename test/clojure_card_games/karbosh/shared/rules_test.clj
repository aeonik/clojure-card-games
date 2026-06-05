(ns clojure-card-games.karbosh.shared.rules-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(deftest effective-suit-test
  (testing "left bower follows trump instead of its printed suit"
    (is (= :♠ (rules/effective-suit [:J :♣] :♠)))
    (is (= :♣ (rules/effective-suit [:J :♣] :♥)))))

(deftest legal-cards-test
  (testing "legal cards collapse duplicate physical copies"
    (let [hand [[:A :♥] [:A :♥] [:K :♠] [9 :♣]]]
      (is (= [[:A :♥] [:K :♠] [9 :♣]]
             (rules/legal-cards hand [] :♠)))
      (is (= [[:A :♥]]
             (rules/legal-cards hand [{:player :player1 :card [:K :♥]}] :♠)))))

  (testing "left bower is treated as trump for follow-suit"
    (let [hand [[:J :♣] [:A :♣] [9 :♥]]
          trick [{:player :player1 :card [:A :♠]}]]
      (is (= [[:J :♣]]
             (rules/legal-cards hand trick :♠))))))

(deftest resolve-trick-test
  (testing "highest effective card wins"
    (is (= :player2
           (rules/resolve-trick [{:player :player1 :card [:A :♠]}
                                 {:player :player2 :card [:J :♠]}
                                 {:player :player3 :card [:J :♣]}]
                                :♠))))

  (testing "earliest equivalent high card wins duplicate-card ties"
    (is (= :player1
           (rules/resolve-trick [{:player :player1 :card [:A :♥]}
                                 {:player :player2 :card [:A :♥]}
                                 {:player :player3 :card [:K :♥]}]
                                :♠)))))
