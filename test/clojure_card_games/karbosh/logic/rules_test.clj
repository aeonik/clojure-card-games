(ns clojure-card-games.karbosh.logic.rules-test
  (:require [clojure.core.logic :as l]
            [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.logic.ai :as logic-ai]
            [clojure-card-games.karbosh.logic.bench :as logic-bench]
            [clojure-card-games.karbosh.logic.rules :as logic-rules]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(defn state
  [{:keys [hands teams active-players current-player current-trick trump]}]
  {:players (into {}
                  (map (fn [[player hand]]
                         [player {:hand hand
                                  :team (get teams player)}])
                       hands))
   :active-players active-players
   :current-player current-player
   :current-trick (vec current-trick)
   :trump trump
   :tricks-this-hand {1 0 2 0}})

(deftest effective-suit-relation-test
  (is (= [:♥]
         (l/run* [suit]
           (logic-rules/effective-suito [:J :♦] :♥ suit))))
  (is (= [:♠]
         (l/run* [suit]
           (logic-rules/effective-suito [:J :♠] :♠ suit))))
  (is (= [:♦]
         (l/run* [suit]
           (logic-rules/effective-suito [:A :♦] :♥ suit)))))

(deftest legal-cards-match-pure-rules-test
  (doseq [context (logic-bench/sample-contexts 120)]
    (is (= (set (rules/legal-cards (:hand context)
                                   (:trick context)
                                   (:trump context)))
           (set (logic-rules/legal-cards (:hand context)
                                         (:trick context)
                                         (:trump context))))
        (pr-str context))))

(deftest relation-legal-cards-collapse-duplicates-test
  (let [hand [[:A :♥] [:A :♥] [:K :♠]]
        trick [{:player :player1 :card [9 :♥]}]]
    (is (= [[:A :♥]]
           (logic-rules/legal-cards hand trick :♣)))))

(deftest winning-play-relation-matches-pure-rules-test
  (let [trick [{:player :player1 :card [:A :♥]}
               {:player :player2 :card [:A :♥]}
               {:player :player3 :card [:J :♦]}]]
    (is (= (rules/winning-play trick :♥)
           (logic-rules/winning-play trick :♥)))
    (is (= :player3
           (:player (logic-rules/winning-play trick :♥))))))

(deftest logic-ai-card-action-plugs-into-bot-strategy-test
  (let [s (state {:hands {:player1 [[:A :♥] [9 :♠]]
                          :player2 [[:K :♥]]}
                  :teams {:player1 1 :player2 2}
                  :active-players [:player1 :player2]
                  :current-player :player1
                  :current-trick []
                  :trump :♠})
        action (bot/card-action s :player1 logic-ai/card-action)]
    (is (= :play-card (:type action)))
    (is (= :core.logic (:engine action)))
    (is (some #{(:card action)}
              (rules/legal-cards (get-in s [:players :player1 :hand])
                                 (:current-trick s)
                                 (:trump s))))))

(deftest bench-reports-equivalent-legal-card-counts-test
  (let [report (logic-bench/bench 50)]
    (is (= 0 (:mismatch-count report)))
    (is (= (get-in report [:pure :card-count])
           (get-in report [:core-logic :card-count])))
    (is (pos? (get-in report [:pure :elapsed-ms])))
    (is (pos? (get-in report [:core-logic :elapsed-ms])))))
