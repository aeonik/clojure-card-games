(ns clojure-card-games.karbosh.solver.play-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.solver.play :as play]))

(defn state
  [{:keys [hands teams active-players current-player current-trick trump tricks]}]
  {:players (into {}
                  (map (fn [[player hand]]
                         [player {:hand hand
                                  :team (get teams player)}])
                       hands))
   :active-players active-players
   :current-player current-player
   :current-trick (or current-trick [])
   :trump trump
   :tricks-this-hand (or tricks {1 0 2 0})})

(defn solve-contract-exhaustive [s contract]
  (let [team (play/contract-team s contract)]
    (play/solve-value-exhaustive
      s
      {:objective [:test-contract contract]
       :max-team team
       :terminal-value #(play/contract-utility % contract)})))

(deftest solve-future-tricks-test
  (testing "solves a forced single trick"
    (let [s (state {:hands {:player1 [[:A :♥]]
                            :player2 [[:K :♥]]}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player1
                    :trump :♠})]
      (is (= 1 (play/solve-future-tricks s 1)))
      (is (= 0 (play/solve-future-tricks s 2)))))

  (testing "keeps earliest equivalent card as trick winner"
    (let [s (state {:hands {:player1 []
                            :player2 [[:A :♥]]}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player2
                    :current-trick [{:player :player1 :card [:A :♥]}]
                    :trump :♠})]
      (is (= 1 (play/solve-future-tricks s 1)))
      (is (= 0 (play/solve-future-tricks s 2)))))

  (testing "current player chooses the best available line for their team"
    (let [s (state {:hands {:player1 [[:A :♥] [:A :♠]]
                            :player2 [[:K :♥] [9 :♠]]}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player1
                    :trump :♠})]
      (is (= 2 (play/solve-future-tricks s 1)))
      (is (= 0 (play/solve-future-tricks s 2))))))

(deftest contract-utility-test
  (let [s (state {:hands {:player1 [] :player2 []}
                  :teams {:player1 1 :player2 2}
                  :active-players [:player1 :player2]
                  :current-player :player1
                  :trump :♠})]
    (testing "numeric bids score point differential from bidder team"
      (is (= 5
             (play/contract-utility
               (assoc s :tricks-this-hand {1 5 2 3})
               {:type :bid :player :player1 :bid-type :bid :value 5})))
      (is (= -9
             (play/contract-utility
               (assoc s :tricks-this-hand {1 5 2 3})
               {:type :bid :player :player1 :bid-type :bid :value 6}))))

    (testing "karbosh is scored as an all-tricks contract"
      (is (= 15
             (play/contract-utility
               (assoc s :tricks-this-hand {1 8 2 0})
               {:type :bid :player :player1 :bid-type :karbosh})))
      (is (= -16
             (play/contract-utility
               (assoc s :tricks-this-hand {1 7 2 1})
               {:type :bid :player :player1 :bid-type :karbosh}))))))

(deftest solve-contract-test
  (testing "solves a forced making bid"
    (let [s (state {:hands {:player1 [[:A :♥]]
                            :player2 [[:K :♥]]}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player1
                    :trump :♠})]
      (is (= 1
             (play/solve-contract
               s
               {:type :bid :player :player1 :bid-type :bid :value 1})))
      (is (= -2
             (play/solve-contract
               s
               {:type :bid :player :player2 :bid-type :bid :value 1})))))

  (testing "solves a forced failed karbosh"
    (let [s (state {:hands {:player1 [[:K :♥]]
                            :player2 [[:A :♥]]}
                    :teams {:player1 1 :player2 2}
                    :active-players [:player1 :player2]
                    :current-player :player1
                    :trump :♠})]
      (is (= -16
             (play/solve-contract
               s
               {:type :bid :player :player1 :bid-type :karbosh}))))))

(deftest alpha-beta-matches-exhaustive-test
  (testing "contract search matches exhaustive minimax"
    (let [s (state {:hands {:player1 [[:A :♥] [9 :♠]]
                            :player2 [[:K :♥] [:A :♠]]
                            :player3 [[:Q :♥] [10 :♠]]}
                    :teams {:player1 1
                            :player2 2
                            :player3 1}
                    :active-players [:player1 :player2 :player3]
                    :current-player :player1
                    :trump :♠})
          contract {:type :bid
                    :player :player1
                    :bid-type :bid
                    :value 2}]
      (is (= (solve-contract-exhaustive s contract)
             (play/solve-contract s contract)))))

  (testing "future-tricks search still matches exhaustive value"
    (let [s (state {:hands {:player1 [[:A :♥] [9 :♠]]
                            :player2 [[:K :♥] [:A :♠]]
                            :player3 [[:Q :♥] [10 :♠]]}
                    :teams {:player1 1
                            :player2 2
                            :player3 1}
                    :active-players [:player1 :player2 :player3]
                    :current-player :player1
                    :trump :♠})
          exhaustive (let [baseline (get-in s [:tricks-this-hand 1] 0)]
                       (- (play/solve-value-exhaustive
                            s
                            {:objective [:test-future-tricks 1]
                             :max-team 1
                             :terminal-value #(get-in % [:tricks-this-hand 1] 0)})
                          baseline))]
      (is (= exhaustive
             (play/solve-future-tricks s 1))))))
