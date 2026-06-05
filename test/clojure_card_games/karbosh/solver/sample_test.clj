(ns clojure-card-games.karbosh.solver.sample-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.solver.sample :as sample]))

(defn all-cards [hands]
  (mapcat val hands))

(deftest sample-hands-test
  (testing "sampling is deterministic for a seed"
    (let [opts {:seed 17
                :players [:player1 :player2]
                :hand-size 2
                :deck [[:A :♥] [:A :♥] [:K :♥] [:Q :♥]]
                :known-hands {:player1 [[:A :♥]]}}
          a (sample/sample-hands opts)
          b (sample/sample-hands opts)]
      (is (= a b))
      (is (= 2 (count (:player1 a))))
      (is (= 2 (count (:player2 a))))
      (is (= [:A :♥] (first (:player1 a))))))

  (testing "known duplicate cards consume only one physical copy"
    (let [hands (sample/sample-hands
                  {:seed 3
                   :known-hands {:player1 [[:A :♥]]}})]
      (is (= 48 (count (all-cards hands))))
      (is (= (frequencies (cards/deck))
             (frequencies (all-cards hands))))))

  (testing "visible non-hand cards are removed from sampled hands"
    (let [hands (sample/sample-hands
                  {:seed 3
                   :known-cards [[:A :♥]]
                   :hand-sizes {:player1 7}})]
      (is (= 47 (count (all-cards hands))))
      (is (= 1 (get (frequencies (all-cards hands)) [:A :♥])))))

  (testing "inconsistent known cards fail clearly"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"not available"
         (sample/sample-hands
           {:known-hands {:player1 [[:A :♥] [:A :♥] [:A :♥]]}})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"exceeds target"
         (sample/sample-hands
           {:known-hands {:player1 [[:A :♥] [:A :♥] [:K :♥]]}
            :hand-sizes {:player1 2}})))))
