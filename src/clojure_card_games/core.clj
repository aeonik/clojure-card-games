(ns clojure-card-games.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.zip :as zip]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))

(def cards [9 10 :J :Q :K :A])

(def suits [:♥ :♠ :♦ :♣])

(def suit-unicode-offset {:♠ 0
                          :♥ 16
                          :♦ 32
                          :♣ 48})
(def rank-unicode-offset {9  9
                          10 10
                          :J 11
                          :Q 13
                          :K 14
                          :A 1})
(def playing-card-code-block 0x1F0A0)

(defn card-to-unicode [[rank suit]]
  (let [suit-offset (suit-unicode-offset suit)
        rank-offset (rank-unicode-offset rank)
        code-point (+ playing-card-code-block suit-offset rank-offset)]
    (String. (Character/toChars code-point))))

[[[[(defn karbosh-deck []
      "Create karbosh deck. Two decks from 9 to Ace of each suit."
      (map vec (mapcat (partial repeat 2)
                       (combo/cartesian-product cards suits))))]]]]

(comment
  (def unicode-deck (map card-to-unicode (karbosh-deck)))
  (def mapped-deck (zipmap (karbosh-deck) unicode-deck)))

^{::clerk/no-cache true}
(defn shuffled-deck []
  "Shuffle the deck."
  (shuffle (karbosh-deck)))

(defn hands []
  "Break the deck into hands with 8 cards each."
  (map vec (partition 8 (shuffled-deck))))

(def players [:player1, :player2, :player3, :player4, :player5, :player6])

(defn rank-values [trump-suit rank]
  (cond
    (= rank :J) (if (= rank trump-suit) 6 7)
    (= rank :left-bower) (if (= trump-suit :♣) 6 7)
    (= rank :right-bower) (if (= trump-suit :♠) 6 7)
    :else (get rank-values rank)))

(def complementary-suits {:♠ :♣,
                          :♣ :♠,
                          :♥ :♦,
                          :♦ :♥})

(defn complement-suit [suit]
  (complementary-suits suit))

(def default-rank-values {:9  0
                          :10 1
                          :J  2
                          :Q  3
                          :K  4
                          :A  5})

(def trump-rank-values {:9           6
                        :10          7
                        :J           6
                        :Q           9
                        :K           10
                        :A           11
                        :left-bower  12
                        :right-bower 13})

(defn rank-of-card [trump-suit card]
  (let [rank (first card)
        suit (second card)
        is-trump (= suit trump-suit)
        is-same-color (= (complementary-suits suit) trump-suit)]
    (cond
      (and is-trump (= rank :J)) (get trump-rank-values :right-bower) ; Right bower
      (and is-same-color (= rank :J)) (get trump-rank-values :left-bower) ; Left bower
      is-trump (get trump-rank-values rank)                 ; Other trump cards
      :else (get default-rank-values rank))))               ; Non-trump cards

(defn card-precedence [trump-suit card1 card2]
  (let [rank1 (rank-of-card trump-suit card1)
        rank2 (rank-of-card trump-suit card2)]
    (cond
      (> rank1 rank2) 1
      (< rank1 rank2) -1
      :else 0)))

(defn compare-cards [trump-suit]
  (fn [card1 card2]
    (- (card-precedence trump-suit card1 card2))))

(def trick {:dealer     :player2
            :bid        8
            :bid-winner :player4
            :trump      :♣
            :cards      [[:J :♣]
                         [:Q :♣]
                         [:A :♣]
                         [:10 :♣]
                         [:9 :♥]
                         [:J :♠]]
            :winner     :team2})

(comment
  (sort-by (fn [card] (rank-of-card :♠ card)) (:cards trick))
  (sort (compare-cards :♣) (:cards trick))
  (complement-suit :♠)
  (complement-suit :♣)
  (complement-suit :♥)
  (complement-suit :♦))

(card-precedence :♣ [:J :♠] [:J :♣])
(card-precedence :♠ [:J :♣] [:J :♠])
(card-precedence :♥ [:A :♥] [:Q :♠])
(card-precedence :♠ [:9 :♣] [:A :♠])

(defn ^:deprecated get-card-rank-value
  [trump-suit card rank-values]
  (let [default-rank (get rank-values (:rank card))]
    (if default-rank
      default-rank
      (if (and (= (:suit card) trump-suit)
               (= (:rank card) :J))
        (get rank-values :left-bower)
        (if (and (= (:suit card) (complementary-suits trump-suit))
                 (= (:rank card) :J))
          (get rank-values :right-bower)
          nil)))))

(def trick [[:J :♠] [:J :♣] [:K :♠]
            [:A :♦] [:Q :♥] [:9 :♣]])

(defn sort-trick [trump-suit trick]
  (sort-by (fn [card] card)
           (partial card-precedence trump-suit)
           trick))

(sort-trick :♠ trick)

(def players-with-hands
  (zipmap players
          (mapv hash-map (repeat :hand) (hands))))

(def game-with-players
  "Thanks to Sean Corfield for this example:
   A more advanced approach would be to represent 
   the changes you want to make as a data structure 
   -- in this case a hash map from player keys to team numbers 
   -- and then reduce over that to make the changes you need:"
  (reduce-kv (fn [game-state player team-number]
               (assoc-in game-state [:game player :team] team-number))
             {:game   players-with-hands
              :tricks []}
             {:player1 1
              :player2 2
              :player3 1
              :player4 2
              :player5 1
              :player6 2}))

^{::clerk/viewer v/table-viewer}
(->> players-with-hands
     (mapv (fn [[player hand-data]]
             [player (update hand-data :hand #(map card-to-unicode %))]))
     (into {}))

;; TODO: Sort player hands by suit and by rank.
;; TODO: When Trump is declared, re-sort bowers.
;; TODO: Write renege detection algorithm.

(defn card-to-unicode-hiccup [card]
  (let [card-color (if (or (= (second card) :♥) (= (second card) :♦))
                     "#f63333"                              ; Softer red color
                     "black")]                              ; Default color for spades and clubs
    [:span {:style {:font-size "120px" :color card-color}} (card-to-unicode card)]))

(defn convert-and-format-hand [hand]
  (map card-to-unicode-hiccup hand))

(def players-with-unicode-hands-hiccup
  (->> players-with-hands
       (mapv (fn [[player hand-data]]
               [player (update hand-data :hand convert-and-format-hand)]))
       (into {})))

(clerk/html
  [:div
   [:style ".hand-container {
              display: flex;
              align-items: flex-start;
              flex-wrap: wrap;
            }
            .card {
              display: flex;
              align-items: flex-start;
              flex-wrap: wrap;
            }"]
   (for [[player hand] players-with-unicode-hands-hiccup]
     [:div
      [:p player]
      [:div.hand-container (for [card hand] [:div.card card])]])])

(clerk/html [:button {:onclick "window.location.reload();"
                      :class "bg-sky-500 hover:bg-sky-700 text-white rounded-xl px-2 py-1"}
             "Refresh Cards"])

(defn set-dealer [game-with-players player]
  "Add player as dealer suit to the game using rand-nth and assoc-in"
  (assoc-in game-with-players [:game :dealer] player))

(defn set-bid [game-with-players player bid]
  "Add player as dealer suit to the game using rand-nth and assoc-in"
  (assoc-in game-with-players [:game :bid] bid))

(defn set-trump [game-with-players trump-suit]
  "Add random trump suit to the game using rand-nth and assoc-in"
  (assoc-in game-with-players [:game :trump] trump-suit))

(defn add-trick [game-with-players trick]
  (assoc-in game-with-players [:tricks] (conj (:tricks game-with-players) trick)))

(defn start-trick [game-with-players last-trick]
  "Create a trick, deal one card from each player's hand to the trick"
  (comment
    "This is a naive way of doing it. It gets the first card of each hand
   Probably want to use Dissoc instead though."
    (map first (map :hand (vals (:game game-with-players))))

    "With threading Macro"
    "TODO - figure out how to get players associed into the trick per card")

  "This should be a function to play the trick
   map first can be substituted with strategies or human input"
  (->> game-with-players
       (:game)
       (vals)
       (map :hand)
       (map first))

  "This line needs a lot of work. Instead of setting things,
    we should generate the trump and bid and set it elsewhere."
  (-> game-with-players
      (set-trump (rand-nth suits))
      (set-bid (rand-nth players) (rand-nth (range 1 9)))))

"BROKEN: Get the every player after player 3 in players
 ROOT CAUSE: This function only works if players-with-hands 
 is a vector; currently it's a hashmap because game-with-players 
 requires this."
(defn get-player-order [game-with-players]
  "This creates a vector that shows the order of the players"
  "BROKEN: players-with-hands was changed to a map"
  (concat
    (subvec players-with-hands 3)
    (subvec players-with-hands 0 3)))

(defn get-player-hands [game-with-players]
  (map #(% (:game game-with-players)) players))

(defn start-game [game-with-players]
  (-> game-with-players
      (set-dealer (rand-nth players))))

(def temp-game
  (start-game game-with-players))

(defn play-cards [game-with-players trick card]
  "Find the dealer first, then use that for the rest of the logic"
  (let [dealer (-> temp-game
                   (:game)
                   (:dealer))]
    (-> game-with-players
        (:game)
        (get-player-hands))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (clojure.pprint/pprint (start-game game-with-players)))