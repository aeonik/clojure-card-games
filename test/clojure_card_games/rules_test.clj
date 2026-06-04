(ns clojure-card-games.rules-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.rules :as rules]))

(deftest bower-test
  (is (rules/right-bower? [:J :♠] :♠))
  (is (rules/left-bower? [:J :♣] :♠))
  (is (= :♠ (rules/effective-suit [:J :♣] :♠))))

(deftest trick-test
  (testing "right bower beats other trump and lead suit"
    (is (= :player3
           (rules/resolve-trick [{:player :player1 :card [:A :♠]}
                                 {:player :player2 :card [:J :♣]}
                                 {:player :player3 :card [:J :♠]}]
                                :♠)))))

(deftest legal-play-test
  (let [hand [[:A :♥] [:A :♠]]
        trick [{:player :player1 :card [:K :♥]}]]
    (is (rules/legal-play? hand trick [:A :♥] :♠))
    (is (not (rules/legal-play? hand trick [:A :♠] :♠)))))

(deftest bid-and-scoring-test
  (is (rules/valid-bid? {:bid-type :bid :value 5}))
  (is (not (rules/valid-bid? {:bid-type :bid :value 9})))
  (is (= {:player :player2 :type :bid :bid-type :bid :value 6}
         (rules/winning-bid [{:player :player1 :type :bid :bid-type :bid :value 5}
                             {:player :player2 :type :bid :bid-type :bid :value 6}
                             {:player :player3 :type :bid :bid-type :pass}])))
  (is (= {1 5 2 0}
         (rules/score-hand {:player1 {:team 1}
                            :player2 {:team 2}}
                           {:player :player1 :bid-type :bid :value 5}
                           {1 5 2 3})))
  (is (= {1 -5 2 4}
         (rules/score-hand {:player1 {:team 1}
                            :player2 {:team 2}}
                           {:player :player1 :bid-type :bid :value 5}
                           {1 4 2 4}))))
