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

(deftest workbench-sorts-hands-before-trump-test
  (let [state {:phase :bidding
               :trump nil}
        hand [[:Q :♣] [:J :♠] [:A :♥] [:J :♣] [9 :♦] [:A :♠]]]
    (is (= [[:J :♠] [:J :♣] [:A :♠] [:A :♥] [:Q :♣] [9 :♦]]
           (workbench/sorted-hand state hand)))))

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

(deftest baseline-table-labels-actor-team-perspective-test
  (let [state {:phase :trick-playing
               :trump :♥
               :hand-index 0
               :bids [{:hand-index 0
                       :type :bid
                       :player :player2
                       :bid-type :bid
                       :value 4}]
               :players (player-states {})}
        session {:room {:game state :seats {}}}
        baseline [{:card [9 :♣]
                   :winner :player4
                   :winner-team 2
                   :actor-team 2
                   :team-wins? true
                   :final-hand {:tricks {1 3 2 5}
                                :scores {1 15 2 5}
                                :actor-team-tricks 5}}]
        rendered (pr-str (workbench/baseline-table-html session baseline))]
    (is (re-find #"Actor team takes current trick\\?" rendered))
    (is (re-find #"Final tricks \(Team 1 / Team 2\)" rendered))
    (is (re-find #"Actor team \(Team 2\) final tricks" rendered))
    (is (re-find #"Team 2 made bid 4 \(5 tricks\)" rendered))
    (is (re-find #"Final score \(Team 1 / Team 2\)" rendered))))

(deftest monte-carlo-panel-renders-actor-team-test
  (let [session {:room {:game {:phase :trick-playing
                               :players (player-states {})}
                        :seats {}}
                 :analysis {:kind :monte-carlo
                            :actor :player2
                            :accepted 0
                            :samples 0
                            :attempts 0
                            :seed 0
                            :baseline []
                            :results {}}}
        rendered (pr-str (workbench/analysis-panel-html session))]
    (is (re-find #"Actor team" rendered))
    (is (re-find #"Team 2" rendered))))
