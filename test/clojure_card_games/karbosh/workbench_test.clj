(ns clojure-card-games.karbosh.workbench-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.workbench :as workbench]))

(defn player-states [hands]
  (into {}
        (map (fn [player]
               [player {:team (get (game/teams) player)
                        :hand (vec (get hands player []))}]))
        game/players))

(deftest ai-view-risk-includes-known-current-trick-cards-test
  (let [state {:phase :trick-playing
               :trump :♥
               :active-players [:player1 :player2]
               :current-player :player2
               :trick-leader :player1
               :current-trick [{:player :player1 :card [:K :♠]}]
               :completed-tricks []
               :players (player-states {:player2 [[9 :♠]]})}
        risks (workbench/probabilistic-card-risks state :player2 [9 :♠] true)]
    (is (= 1.0 (:any risks)))
    (is (= 1.0 (:opponent risks)))))

(deftest ai-view-risk-distinguishes-partner-current-trick-control-test
  (let [state {:phase :trick-playing
               :trump :♥
               :active-players [:player1 :player3]
               :current-player :player3
               :trick-leader :player1
               :current-trick [{:player :player1 :card [:K :♠]}]
               :completed-tricks []
               :players (player-states {:player3 [[9 :♠]]})}
        risks (workbench/probabilistic-card-risks state :player3 [9 :♠] true)]
    (is (= 1.0 (:any risks)))
    (is (= 0.0 (:opponent risks)))))
