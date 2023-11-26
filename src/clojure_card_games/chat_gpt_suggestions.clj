(ns clojure-card-games.chat-gpt-suggestions
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.zip :as zip]
            [clojure.core.cards :as c]))

(c/defcard 9)
(c/defcard 10)
(c/defcard :J)
(c/defcard :Q)
(c/defcard :K)
(c/defcard :A)

(c/defsuit :♥)
(c/defsuit :♠)
(c/defsuit :♦)
(c/defsuit :♣)

(defn karbosh-deck []
  "Create a karbosh deck. Two decks from 9 to Ace of each suit."
  (map c/card (combo/cartesian-product (c/cards) (c/suits))))

(defn hands []
  "Break the deck into hands with 8 cards each."
  (map vec (partition 8 (shuffle (karbosh-deck)))))

(def players [:player1, :player2, :player3, :player4, :player5, :player6])

(def players-with-hands
  (zipmap players
          (mapv #(hash-map :hand %) (hands))))

(def game-with-players
  (merge-with (fn [game player] (assoc game :team (player :team)))
              {:game players-with-hands}
              {:player1 {:team 1}
               :player2 {:team 2}
               :player3 {:team 1}
               :player4 {:team 2}
               :player5 {:team 1}
               :player6 {:team 2}}))

(defn set-trump [game-with-players trump-suit]
  "Add random trump suit to the game using rand-nth and update-in"
  (update-in game-with-players [:game :trump] (constantly trump-suit)))

(defn set-dealer [game-with-players player]
  "Add player as dealer suit to the game using update-in"
  (update-in game-with-players [:game :dealer] (constantly player)))

(defn set-bid [game-with-players player bid]
  "Add player's bid to the game using update-in"
  (update-in game-with-players [:game :bid] (constantly bid)))

(defn start-trick [game-with-players]
  "Create a trick, deal one card from each player's hand to the trick"
  (->> game-with-players
       (:game)
       (vals)
       (map :hand)
       (map first)))