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

(deftest legal-play-test
  (testing "players must follow suit when able"
    (let [hand [[:A :♥] [:A :♠]]
          trick [{:player :player1 :card [:K :♥]}]]
      (is (rules/legal-play? hand trick [:A :♥] :♠))
      (is (not (rules/legal-play? hand trick [:A :♠] :♠))))))

(deftest bid-validation-test
  (is (rules/valid-bid? {:bid-type :bid :value 5}))
  (is (not (rules/valid-bid? {:bid-type :bid :value 9})))
  (is (= {:player :player2 :type :bid :bid-type :bid :value 6}
         (rules/winning-bid [{:player :player1 :type :bid :bid-type :bid :value 5}
                             {:player :player2 :type :bid :bid-type :bid :value 6}
                             {:player :player3 :type :bid :bid-type :pass}]))))

(deftest score-hand-test
  (let [players {:player1 {:team 1} :player2 {:team 2}}]
    (testing "made numeric bids score the bidder's tricks"
      (is (= {1 5 2 0}
             (rules/score-hand players
                               {:player :player1 :bid-type :bid :value 5}
                               {1 5 2 3}))))

    (testing "set numeric bids go negative and defenders keep tricks"
      (is (= {1 -5 2 4}
             (rules/score-hand players
                               {:player :player1 :bid-type :bid :value 5}
                               {1 4 2 4}))))

    (testing "karbosh and double karbosh both score 15 by house rule"
      (doseq [bid-type [:karbosh :double-karbosh]]
        (is (= {1 15 2 0}
               (rules/score-hand players
                                 {:player :player1 :bid-type bid-type}
                                 {1 8 2 0})))
        (is (= {1 -15 2 1}
               (rules/score-hand players
                                 {:player :player1 :bid-type bid-type}
                                 {1 7 2 1})))))))

(deftest legal-bid-test
  (testing "non-pass bids must strictly outrank the current bid"
    (let [current {:type :bid :player :player1 :bid-type :bid :value 5}]
      (is (true? (rules/legal-bid? current {:bid-type :pass})))
      (is (false? (rules/legal-bid? current {:bid-type :bid :value 4})))
      (is (false? (rules/legal-bid? current {:bid-type :bid :value 5})))
      (is (true? (rules/legal-bid? current {:bid-type :bid :value 6})))
      (is (true? (rules/legal-bid? current {:bid-type :karbosh})))
      (is (true? (rules/legal-bid? {:bid-type :karbosh}
                                    {:bid-type :double-karbosh})))
      (is (false? (rules/legal-bid? {:bid-type :karbosh}
                                     {:bid-type :karbosh}))))))
