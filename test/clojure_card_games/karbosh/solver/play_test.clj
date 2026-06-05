(ns clojure-card-games.karbosh.solver.play-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.solver.play :as play]))

(defn state
  [{:keys [hands teams active-players current-player current-trick trump]}]
  {:players (into {}
                  (map (fn [[player hand]]
                         [player {:hand hand
                                  :team (get teams player)}])
                       hands))
   :active-players active-players
   :current-player current-player
   :current-trick (or current-trick [])
   :trump trump
   :tricks-this-hand {1 0 2 0}})

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
