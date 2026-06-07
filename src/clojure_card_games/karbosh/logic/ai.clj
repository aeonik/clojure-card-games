(ns clojure-card-games.karbosh.logic.ai
  (:require [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.logic.rules :as logic-rules]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(defn legal-cards [game player]
  (logic-rules/legal-cards (get-in game [:players player :hand])
                           (:current-trick game)
                           (:trump game)))

(defn winning-cards [game player]
  (logic-rules/legal-winning-cards (get-in game [:players player :hand])
                                   (:current-trick game)
                                   (:trump game)
                                   player))

(defn current-trick-winner [game]
  (when (seq (:current-trick game))
    (:player (logic-rules/winning-play (:current-trick game)
                                       (:trump game)))))

(defn card-action
  "A small pluggable play strategy using core.logic for move legality.

  It intentionally keeps tactical choice simple. The useful experiment is
  whether relation-derived legal moves can satisfy the same strategy interface
  as the regular bot AI:

    (bot/card-action game player clojure-card-games.karbosh.logic.ai/card-action)"
  [game player]
  (let [cards (vec (legal-cards game player))
        winner (current-trick-winner game)
        winning-cards (vec (winning-cards game player))
        card (cond
               (empty? cards)
               nil

               (empty? (:current-trick game))
               (bot/lowest-card game cards)

               (bot/same-team? game player winner)
               (bot/lowest-card game cards)

               (seq winning-cards)
               (bot/lowest-card game winning-cards)

               :else
               (bot/lowest-card game cards))]
    (when card
      {:type :play-card
       :card card
       :engine :core.logic
       :legal-card-count (count cards)})))

(defn equivalent-legal-cards? [game player]
  (= (set (rules/legal-cards (get-in game [:players player :hand])
                             (:current-trick game)
                             (:trump game)))
     (set (legal-cards game player))))
