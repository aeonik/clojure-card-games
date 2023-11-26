(ns clojure-card-games.core-test
  (:require [clojure.test :refer :all]
            [clojure-card-games.core :refer :all]))

(deftest card-precedence-test
  (testing "Trump-suit precedence over higher rank non-trump-suit"
    (is (= 1 (card-precedence :♠
                              [:J :♠]
                              [:A :♣]))
        "Expected J♠ to beat K♣ when Spades is trump")
    (is (= -1 (card-precedence :♠
                               [:A :♣]
                               [:J :♠]))
        "Expected K♣ to lose to J♠ when Spades is trump")

    (is (= 1 (card-precedence :♠
                              [:J :♠]
                              [:A :♣]))
        "Expected J♠ to beat A♣ when Spades is trump")
    (is (= -1 (card-precedence :♠
                               [:A :♣]
                               [:J :♠]))
        "Expected A♣ to lose to J♠ when Spades is trump"))

  (testing "Non-trump suit higher rank beats lower rank"
    (is (= 1 (card-precedence :♠
                              [:A :♥]
                              [:K :♣]))
        "Expected :A♥ to beat K♣ when Spades is trump")
    (is (= -1 (card-precedence :♠
                              [:J :♥]
                              [:A :♣]))))

  (testing "Trump suit beats same rank card in non-trump suit"
    (testing "Spades as trump"
      (is (= 1 (card-precedence :♠
                                [:J :♠]
                                [:J :♥]))
          "Expected J♠ to beat J♣ when Spades is trump")
      (is (= -1 (card-precedence :♠
                                 [:J :♥]
                                 [:J :♠]))
          "Expected J♣ to lose to J♠ when Spades is trump"))

    (testing "Clubs as trump"
      (is (= 1 (card-precedence :♣
                                [:J :♣]
                                [:J :♥]))
          "Expected J♣ to beat J♠ when Clubs is trump")
      (is (= -1 (card-precedence :♣
                                 [:J :♥]
                                 [:J :♣]))
          "Expected J♠ to lose to J♣ when Clubs is trump"))))

(deftest compare-cards-sort-test
  (testing "Cards sorted in descending order according to trump suit"
    (let [trump-suit :♠
          hand-unsorted [[:J :♣] [:Q :♣] [:A :♣] [:10 :♣] [:9 :♥] [:J :♠]]
          hand-sorted (sort (compare-cards trump-suit) hand-unsorted)]

      (testing "Highest trump card comes first"
        (is (= (first hand-sorted) [:J :♠])))

      (testing "Non-trump cards sorted by rank after trump cards"
        (is (= (last hand-sorted) [:9 :♥])))

      (testing "Full hand is sorted correctly"
        (is (= hand-sorted [[:J :♠] [:J :♣] [:A :♣] [:Q :♣] [:10 :♣] [:9 :♥]]))))))