(ns clojure-card-games.state
  (:require [clojure-card-games.cards :as cards]
            [clojure-card-games.deck :as deck]
            [clojure-card-games.rules :as rules]))

(def players [:player1 :player2 :player3 :player4 :player5 :player6])
(def target-score 52)

(defn teams []
  (zipmap players (cycle [1 2])))

(defn derive-seed [state]
  (hash [(:initial-seed state) (inc (:hand-index state))]))

(defn current-bidder [state]
  (get-in state [:bidding-order (:current-bidder-index state)]))

(defn next-player [player]
  (get players (mod (inc (.indexOf players player)) (count players))))

(defn remove-first
  [x coll]
  (let [[before after] (split-with #(not= x %) coll)]
    (vec (concat before (rest after)))))

(defn- fail [message data]
  (throw (ex-info message data)))

(defn- require-phase [state phase event]
  (when-not (= phase (:phase state))
    (fail "Event is not valid in this phase"
          {:expected phase :actual (:phase state) :event event})))

(defn- require-current-player [state player event]
  (when-not (= player (:current-player state))
    (fail "Not this player's turn"
          {:expected (:current-player state) :actual player :event event})))

(defn- complete-hand [state]
  (let [bid (:current-bid state)
        tricks (:tricks-this-hand state)
        points (if bid
                 (rules/score-hand (:players state) bid tricks)
                 {1 0 2 0})
        scores (merge-with + (:scores state) points)
        winner (some (fn [[team score]]
                       (when (>= score target-score) team))
                     scores)]
    (cond-> state
      true
      (assoc :phase (if winner :game-over :hand-complete)
             :scores scores
             :tricks-this-hand {1 0 2 0})
      winner
      (assoc :winner winner)
      true
      (update :tricks-per-hand conj tricks)
      true
      (update :points-per-hand conj points))))

(defn init-game
  ([] (init-game nil 0 [] [] [] [] :player1 nil))
  ([seed] (init-game seed 0 [] [] [] [] :player1 seed))
  ([seed hand-index bids trumps tricks-per-hand points-per-hand dealer]
   (init-game seed hand-index bids trumps tricks-per-hand points-per-hand dealer seed))
  ([seed hand-index bids trumps tricks-per-hand points-per-hand dealer initial-seed]
   (let [deck (deck/shuffle-deck (deck/karbosh-deck) seed)
         hands (deck/deal-hands deck)
         teams (teams)]
     {:phase :bidding
      :history []
      :deck deck
      :players (zipmap players
                       (map (fn [hand _ team]
                              {:hand hand :team team})
                            hands players (vals teams)))
      :scores {1 0, 2 0}
      :current-bid nil
      :bidding-order (vec players)
      :current-bidder-index 0
      :current-player (first players)
      :completed-tricks []
      :bids bids
      :trumps trumps
      :tricks-per-hand tricks-per-hand
      :points-per-hand points-per-hand
      :hand-index hand-index
      :dealer dealer
      :tricks-this-hand {1 0, 2 0}
      :initial-seed initial-seed})))

(defmulti apply-event (fn [_ event] (:type event)))

(defmethod apply-event :bid [state {:keys [player] :as event}]
  (require-phase state :bidding event)
  (when-not (= player (current-bidder state))
    (fail "Not this player's turn to bid"
          {:expected (current-bidder state) :actual player :event event}))
  (when-not (rules/valid-bid? event)
    (fail "Invalid bid" {:event event}))
  (let [event (select-keys event [:type :player :bid-type :value])
        hand-index (:hand-index state)
        updated-state (-> state
                          (update :history conj event)
                          (update :bids conj (assoc event :hand-index hand-index)))
        bids-this-hand (filter #(= hand-index (:hand-index %)) (:bids updated-state))
        complete? (or (= :double-karbosh (:bid-type event))
                      (= (count bids-this-hand)
                         (count (:bidding-order updated-state))))
        winning-bid (rules/winning-bid bids-this-hand)
        next-bidder-idx (mod (inc (:current-bidder-index updated-state))
                             (count (:bidding-order updated-state)))]
    (if complete?
      (if winning-bid
        (assoc updated-state
               :phase :trump-selection
               :current-bid (dissoc winning-bid :hand-index)
               :current-player (:player winning-bid))
        (assoc updated-state
               :phase :hand-complete
               :current-bid nil))
      (assoc updated-state
             :current-bid event
             :current-bidder-index next-bidder-idx
             :current-player (get-in updated-state [:bidding-order next-bidder-idx])))))

(defmethod apply-event :trump-selection [state {:keys [player suit] :as event}]
  (require-phase state :trump-selection event)
  (require-current-player state player event)
  (when-not (contains? (set (keys cards/suit->str)) suit)
    (fail "Invalid trump suit" {:suit suit :event event}))
  (-> state
      (update :history conj event)
      (assoc :trump suit)
      (update :trumps conj suit)
      (assoc :phase :trick-playing)
      (assoc :current-trick [])
      (assoc :trick-leader player)
      (assoc :current-player player)))

(defmethod apply-event :play-card [state {:keys [player card] :as event}]
  (require-phase state :trick-playing event)
  (require-current-player state player event)
  (let [current-trick (:current-trick state)
        player-hand (get-in state [:players player :hand])
        _ (when-not (rules/legal-play? player-hand current-trick card (:trump state))
            (fail "Illegal card play" {:player player :card card :event event}))
        next-player (next-player player)
        updated-trick (conj current-trick {:player player :card card})
        trick-complete? (= (count updated-trick) 6)
        state-after-play (-> state
                             (update :history conj event)
                             (update-in [:players player :hand]
                                        #(vec (remove-first card %))))
        hands-empty (every? (comp empty? :hand) (vals (get state-after-play :players)))]
    (cond
      trick-complete?
      (let [winner (rules/resolve-trick updated-trick (:trump state))
            winner-team (get-in state [:players winner :team])]
        (cond-> state-after-play
          true
          (update-in [:tricks-this-hand winner-team] inc)
          true
          (assoc :current-trick []
                 :current-player winner)
          true
          (update :completed-tricks conj updated-trick)
          hands-empty
          complete-hand))

      :else
      (-> state-after-play
          (assoc :current-trick updated-trick)
          (assoc :current-player next-player)))))

(defmethod apply-event :new-hand [state _]
  (let [next-hand-index (inc (:hand-index state))
        bids (:bids state)
        trumps (:trumps state)
        tricks-per-hand (:tricks-per-hand state)
        points-per-hand (:points-per-hand state)
        scores (:scores state)
        current-dealer (:dealer state)
        next-dealer (get players (mod (inc (.indexOf players current-dealer)) (count players)))
        initial-seed (:initial-seed state)
        new-seed (hash [initial-seed next-hand-index])]
    (-> (init-game new-seed next-hand-index bids trumps tricks-per-hand points-per-hand next-dealer initial-seed)
        (assoc :scores scores))))

(defmethod apply-event :default [_ event]
  (fail "Unknown event type" {:event event}))
