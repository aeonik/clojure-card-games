(ns clojure-card-games.cards-deck-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.cards :as cards]
            [clojure-card-games.deck :as deck]
            [clojure-card-games.karbosh.shared.cards :as karbosh-cards]))

(deftest card-data-test
  (is (= "A" (cards/rank->str :A)))
  (is (= :♥ (cards/str->suit "♥")))
  (is (= "🂻" (cards/unicode [:J :♥])))
  (is (= "🂽" (cards/unicode [:Q :♥])))
  (is (= [10 :♦] (cards/parse-card "0d")))
  (is (= [:Q :♥] (cards/parse-card "Q♥"))))

(deftest build-deck-test
  (testing "default spec builds a single 52-card deck"
    (let [deck (deck/build-deck)]
      (is (= 52 (count deck)))
      (is (= 52 (count (set deck))))))

  (testing "karbosh uses two copies of each 9-A card"
    (let [deck (karbosh-cards/deck)]
      (is (= 48 (count deck)))
      (is (= 24 (count (set deck))))
      (is (= 2 (count (filter #{[:A :♥]} deck))))))

  (testing "seeded shuffles are reproducible"
    (is (= (deck/shuffle-deck (karbosh-cards/deck) 42)
           (deck/shuffle-deck (karbosh-cards/deck) 42))))

  (testing "dealing produces six eight-card hands"
    (let [hands (karbosh-cards/deal (karbosh-cards/deck))]
      (is (= 6 (count hands)))
      (is (every? #(= 8 (count %)) hands)))))

(deftest karbosh-parse-card-test
  (testing "ranks outside the karbosh deck do not parse"
    (is (= [9 :♣] (karbosh-cards/parse-card "9c")))
    (is (nil? (karbosh-cards/parse-card "5c")))))
