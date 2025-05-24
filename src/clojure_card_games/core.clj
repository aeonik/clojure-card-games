(ns clojure-card-games.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]
            [clojure.zip :as zip]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn cards []
  "Karbosh cards."
  (vector 9 10 :J :Q :K :A))

(defn suits []
  "Karbosh suits."
  (vector :♥ :♠ :♦ :♣))

(defn karbosh-deck []
  "Create karbosh deck. Two decks from 9 to Ace of each suit."
  (map vec (mapcat (partial repeat 2)
                   (combo/cartesian-product (cards) (suits)))))

(def shuffled-deck
  "Shuffle the deck."
  (shuffle (karbosh-deck)))

(defn init-deck []
  (shuffle (karbosh-deck)))

(defn deal-hands [deck]
  (map vec (partition 8 deck)))

(def hands
  "Break the deck into hands with 8 cards each."
  (map vec (partition 8 shuffled-deck)))

(def players [:player1, :player2, :player3, :player4, :player5, :player6])

(def players-with-hands
  (zipmap players
          (mapv hash-map (repeat :hand) hands)))

(comment
  "This is my old way of doing it. Not idiomatic in Clojure.
   From Sean Cornfield: The first bit of advice I'd offer is: don't treat def like an assignment would be in other languages. 
   First off, def always introduces a global (top-level) Var -- you don't show much of your code but you should never use def inside a function.
   We generally think of def as introducing a single global constant.
   Second, for repeated operations on a single value where we want to accumulate the result, take a look at -> : "
  (def players-with-hands (assoc {} :game players-with-hands))
  (def players-with-hands (assoc-in players-with-hands [:game :player1 :team] 1))
  (def players-with-hands (assoc-in players-with-hands [:game :player2 :team] 2))
  (def players-with-hands (assoc-in players-with-hands [:game :player3 :team] 1))
  (def players-with-hands (assoc-in players-with-hands [:game :player4 :team] 2))
  (def players-with-hands (assoc-in players-with-hands [:game :player5 :team] 1))
  (def players-with-hands (assoc-in players-with-hands [:game :player6 :team] 2)))

(comment
  "Sean Cornfield's slightly better way of doing it."
  (def game-with-players
    (-> {:game players-with-hands}
        (assoc-in [:game :player1 :team] 1)
        (assoc-in [:game :player2 :team] 2)
        (assoc-in [:game :player3 :team] 1)
        (assoc-in [:game :player4 :team] 2)
        (assoc-in [:game :player5 :team] 1)
        (assoc-in [:game :player6 :team] 2))))

(def game-with-players
  "Thanks to Sean Cornfield for this example: 
   A more advanced approach would be to represent 
   the changes you want to make as a data structure 
   -- in this case a hash map from player keys to team numbers 
   -- and then reduce over that to make the changes you need:"
  (reduce-kv (fn [game-state player team-number]
               (assoc-in game-state [:game player :team] team-number))
             {:game players-with-hands}
             {:player1 1
              :player2 2
              :player3 1
              :player4 2
              :player5 1
              :player6 2}))

(defn set-trump [game-state trump-suit]
  (assoc-in game-state [:game :trump] trump-suit))

;; Usage:
;; (set-trump game-state :♠)

(defn set-dealer [game-state dealer]
  (assoc-in game-state [:game :dealer] dealer))

(defn set-bid
  "Sets the bid in the game state.

   Args:
   - `game-state` (map): The current game state.
   - `player` (keyword): The player making the bid.
   - `bid` (int): The value of the bid.

   Returns:
   - `updated-game-state` (map): The game state with the updated bid information."
  [game-state player bid]
  (assoc-in game-state [:game :bid] {:player player :value bid}))

(defn start-trick [game-state]
  (assoc-in game-state [:game :current-trick] []))

(defn generate-trick [game-state]
  "Simulates a trick by having each player in turn play their first card.
   Updates the game state and returns the generated trick."
  (let [turn-order (get-in game-state [:game :turn-order])]
    (reduce
      (fn [state player]
        (let [card (first (get-in state [:game :players player :hand]))]
          (if card
            (-> state
                (update-in [:game :players player :hand] rest)
                (update-in [:game :current-trick] conj {:player player :card card}))
            (throw (ex-info "Player ran out of cards" {:player player})))))
      (assoc-in game-state [:game :current-trick] [])
      turn-order)))

(defn play-card-for-trick [game-state player card]
  (if (= player (get-in game-state [:game :current-turn]))
    (let [next-turn (->> (get-in game-state [:game :turn-order])
                         (cycle)
                         (drop-while #(not= % player))
                         (second))
          updated-state (-> game-state
                            (update-in [:game :players player :hand] #(remove #{card} %))
                            (update-in [:game :current-trick] conj {:player player :card card})
                            (assoc-in [:game :current-turn] next-turn))]
      (if (= (count (get-in updated-state [:game :current-trick]))
             (count (get-in updated-state [:game :turn-order])))
        ;; If the trick is complete, move it to :tricks
        (-> updated-state
            (update-in [:game :tricks] conj (get-in updated-state [:game :current-trick]))
            (assoc-in [:game :current-trick] []))
        ;; Otherwise, return the updated state
        updated-state))
    (throw (ex-info "Not this player's turn!" {:player player}))))

(defn resolve-trick
  "Determines the winner of a trick based on the highest-ranking card.
   Trump cards outrank all others, followed by cards of the lead suit.

   Args:
   - `trick` (seq): A sequence of maps, each containing `:player` and `:card` ([rank suit]).
   - `trump` (keyword): The trump suit.

   Returns:
   - `winner` (keyword): The player who wins the trick."
  [trick trump]
  (let [lead-suit (-> trick first :card second)
        rank-value {9 0, 10 1, :J 2, :Q 3, :K 4, :A 5}]
    (->> trick
         ;; Filter by trump cards or lead suit
         (filter (fn [{:keys [card]}]
                   (let [suit (second card)]
                     (or (= suit trump) (= suit lead-suit)))))
         ;; Sort by rank, prioritizing trump cards
         (sort-by (fn [{:keys [card]}]
                    (let [[rank suit] card]
                      (cond
                        (= suit trump) (+ 100 (rank-value rank)) ; Trump has highest priority
                        (= suit lead-suit) (rank-value rank)     ; Lead suit next
                        :else -1)))                             ; Exclude other suits
                  >)
         first
         :player)))
(defn score-trick [game-state trick]
  (let [trump (get-in game-state [:game :trump])
        winner (resolve-trick trick trump)
        winner-team (get-in game-state [:game :players winner :team])]
    (update-in game-state [:game :scores winner-team] (fnil inc 0))))

(defn get-player-order [players current-player]
  (let [player-idx (.indexOf players current-player)]
    (concat (drop player-idx players) (take player-idx players))))

(defn get-player-hands [game-with-players]
  (map #(% (:game game-with-players)) players))

(defn start-new-hand [game-state]
  (let [deck (init-deck)
        hands (deal-hands deck)]
    (-> game-state
        (update :game assoc :hands (conj (get-in game-state [:game :hands])
                                         (get-in game-state [:game :current-hand])))
        (assoc-in [:game :current-hand]
                  {:tricks []
                   :players (zipmap players
                                    (mapv (fn [hand team]
                                            {:hand hand
                                             :team team
                                             :score (get-in game-state [:game :scores team])})
                                          hands
                                          (cycle [1 2])))
                   :trump (rand-nth [:♥ :♠ :♦ :♣])
                   :turn-order players
                   :current-turn (first players)}))))

(defn play-card
  "Processes the action of a player playing a card during their turn.
   Updates the game state to reflect the move and advances the turn order.

   Args:
   - `game-state` (map): The current state of the game.
     - Includes player hands, the current trick, turn order, and current turn.
   - `player` (keyword): The player attempting to play a card.
   - `card` (vector): The card the player wants to play, represented as [rank suit].

   Returns:
   - `game-state` (map): The updated game state after the move.
     - The played card is removed from the player's hand.
     - The card is added to the `:current-trick`.
     - The `:current-turn` advances to the next player.

   Example:
   ;; Initial game state
   (def game-state
     {:game {:players {:player1 {:hand [[9 :♥] [10 :♠]]}
                      :player2 {:hand [[:J :♠] [:Q :♦]]}
                      :player3 {:hand [[:K :♣] [:A :♠]]}}
             :current-trick []
             :turn-order [:player1 :player2 :player3]
             :current-turn :player1}})

   ;; Call
   (play-card game-state :player1 [9 :♥])

   ;; Result
   {:game {:players {:player1 {:hand [[10 :♠]]}
                    :player2 {:hand [[:J :♠] [:Q :♦]]}
                    :player3 {:hand [[:K :♣] [:A :♠]]}}
           :current-trick [{:player :player1 :card [9 :♥]}]
           :turn-order [:player1 :player2 :player3]
           :current-turn :player2}}"
  [game-state player card]
  (let [turn-order (get-in game-state [:game :turn-order])
        next-turn (->> turn-order
                       (cycle)
                       (drop-while #(not= % player))
                       (second))]
    (-> game-state
        (update-in [:game :players player :hand] #(remove #{card} %))
        (update-in [:game :current-trick] conj {:player player :card card})
        (assoc-in [:game :current-turn] next-turn))))

(defn game-over?
  "Determines if the game is over based on team scores.

   Args:
   - `game-state` (map): The current game state.

   Returns:
   - `result` (map): A map with keys:
     - `:over` (boolean): True if the game is over.
     - `:winner` (int or nil): The winning team (if any).
     - `:scores` (map): The scores for each team."
  [game-state]
  (let [scores (reduce
                 (fn [team-scores {:keys [team score]}]
                   (update team-scores team (fnil + 0) score))
                 {}
                 (map second (get-in game-state [:game :players])))
        winning-team (some (fn [[team score]]
                             (when (>= score 52)
                               team))
                           scores)]
    {:over (boolean winning-team)
     :winner winning-team
     :scores scores}))

(defn start-game [game-state dealer trump bid]
  (-> game-state
      (set-dealer dealer)
      (set-trump trump)
      (set-bid (:player bid) (:value bid))))

;; Initialize the game state
(defn init-game-state []
  (let [deck (init-deck)
        hands (deal-hands deck)]
    {:game {:deck deck
            :hands []             ; History of completed hands
            :current-hand {:tricks []           ; Tricks played in the current hand
                           :players (zipmap players
                                            (mapv (fn [hand team]
                                                    {:hand hand
                                                     :team team
                                                     :score 0}) ; Start scores at 0
                                                  hands
                                                  (cycle [1 2])))
                           :trump (rand-nth [:♥ :♠ :♦ :♣]) ; Random trump suit
                           :turn-order players  ; Player turn order
                           :current-turn (first players)} ; First player's turn
            :scores {1 0, 2 0}}})) ; Overall team scores

;; Run a game round
(let [game-state (init-game-state)]
  (-> game-state
      (set-trump :♠)
      (set-dealer :player1)
      (start-trick)))

(defn game-loop [game-state]
  (loop [state game-state]
    (if-let [winning-team (some (fn [[team score]]
                                  (when (>= score 52) team))
                                (get-in state [:game :scores]))]
      ;; Game over, return final state
      {:winner winning-team
       :final-scores (get-in state [:game :scores])
       :tricks (get-in state [:game :tricks])}
      ;; Otherwise, continue playing
      (let [state-with-new-trick (start-trick state)]
        ;; Simulate trick plays for now
        (recur (reduce (fn [state player]
                         (let [card (first (get-in state [:game :players player :hand]))]
                           (play-card-for-trick state player card)))
                       state-with-new-trick
                       (get-in state [:game :turn-order])))))))

(defn play-game [initial-state]
  (loop [game-state initial-state]
    (if-let [winning-team (some (fn [[team score]]
                                  (when (>= score 52) team))
                                (get-in game-state [:game :scores]))]
      {:winner winning-team
       :final-scores (get-in game-state [:game :scores])
       :tricks (get-in game-state [:game :tricks])}
      (let [new-trick (generate-trick game-state)] ;; generate-trick simulates a trick
        (recur (-> game-state
                   (update-in [:game :tricks] conj new-trick)
                   (score-trick new-trick)))))))

(defn simulate-games [n initial-state]
  (reduce (fn [stats _]
            (let [result (play-game initial-state)
                  winner (:winner result)]
              (update stats winner (fnil inc 0))))
          {:team-1 0 :team-2 0}
          (range n)))