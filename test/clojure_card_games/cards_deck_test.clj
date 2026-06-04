(ns clojure-card-games.cards-deck-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.cards :as cards]
            [clojure-card-games.deck :as deck]))

(deftest card-data-test
  (is (= "A" (cards/rank->str :A)))
  (is (= :♥ (cards/str->suit "♥")))
  (is (= "🂻" (cards/unicode [:J :♥]))))

(deftest deck-test
  (testing "karbosh uses two copies of each 9-A card"
    (let [deck (deck/karbosh-deck)]
      (is (= 48 (count deck)))
      (is (= 24 (count (set deck))))
      (is (= 2 (count (filter #{[:A :♥]} deck))))))

  (testing "seeded shuffles are reproducible"
    (is (= (deck/shuffle-deck (deck/karbosh-deck) 42)
           (deck/shuffle-deck (deck/karbosh-deck) 42))))

  (testing "dealing produces six eight-card hands"
    (let [hands (deck/deal-hands (deck/karbosh-deck))]
      (is (= 6 (count hands)))
      (is (every? #(= 8 (count %)) hands)))))
