(ns clojure-card-games.state
  (:require [clojure-card-games.deck :as deck]
            [clojure-card-games.hand :as hand]
            [clojure-card-games.rules :as rules]))

;; Pure game state transitions (no side effects)

(defn derive-seed [state]
  (let [initial-seed (:initial-seed state)
        hand-index (:hand-index state)]
    (hash [initial-seed (inc hand-index)])))

(defn init-game
  ([] (init-game nil 0 [] [] [] [] :player1 nil))
  ([seed] (init-game seed 0 [] [] [] [] :player1 seed))
  ([seed hand-index bids trumps tricks-per-hand points-per-hand dealer]
   (init-game seed hand-index bids trumps tricks-per-hand points-per-hand dealer seed))
  ([seed hand-index bids trumps tricks-per-hand points-per-hand dealer initial-seed]
   (let [deck (deck/shuffle-deck (deck/karbosh-deck) seed)
         hands (deck/deal-hands deck)
         players [:player1 :player2 :player3 :player4 :player5 :player6]
         teams (zipmap players (cycle [1 2]))]
     {:phase :bidding
      :history []
      :deck deck
      :players (zipmap players
                       (map (fn [hand player team]
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

(defmulti apply-event (fn [state event] (:type event)))

(defmethod apply-event :bid [state {:keys [player bid-type value] :as event}]
  (let [current-bidder (get-in state [:bidding-order (:current-bidder-index state)])
        hand-index (:hand-index state)
        updated-state (-> state
                          (update :history conj event)
                          (update :bids conj (assoc event :hand-index hand-index)))
        all-bids (->> (:history updated-state)
                      (filter #(and (= (:type %) :bid)
                                    (not= :pass (:bid-type %))))
                      (sort-by :value >))
        bidding-complete? (or (= (:bid-type event) :double-karbosh)
                              (= (count (:history updated-state)) (count (:bidding-order updated-state))))
        winning-bid (first all-bids)
        winning-bidder (:player winning-bid)
        next-bidder-idx (mod (inc (:current-bidder-index updated-state)) (count (:bidding-order updated-state)))]
    (cond-> updated-state
      (not bidding-complete?)
      (assoc :current-bid event)
      bidding-complete?
      (assoc :current-bid winning-bid)
      (not bidding-complete?)
      (assoc :current-bidder-index next-bidder-idx
             :current-player (get-in updated-state [:bidding-order next-bidder-idx]))
      bidding-complete?
      (assoc :phase (if (#{:karbosh :double-karbosh} (:bid-type winning-bid)) :hand-complete :trump-selection)
             :current-player winning-bidder))))

(defmethod apply-event :trump-selection [state {:keys [player suit] :as event}]
  (-> state
      (update :history conj event)
      (assoc :trump suit)
      (update :trumps conj suit)
      (assoc :phase :trick-playing)
      (assoc :current-trick [])
      (assoc :trick-leader player)
      (assoc :current-player player)))

(defn remove-first
  "Return `coll` with only the *first* item equal to `x` removed."
  [x coll]
  (let [[before after] (split-with #(not= x %) coll)]
    (concat before (rest after))))

(defmethod apply-event :play-card [state {:keys [player card] :as event}]
  (let [current-trick (:current-trick state)
        player-hand (get-in state [:players player :hand])
        next-player (let [players (vec (keys (:players state)))
                          idx (.indexOf players player)]
                      (get players (mod (inc idx) (count players))))
        updated-trick (conj current-trick {:player player :card card})
        trick-complete? (= (count updated-trick) 6)
        state-after-play (-> state
                             (update :history conj event)
                             (update-in [:players player :hand]
                                        #(vec (remove-first card %))))
        hands-empty (every? (comp empty? :hand) (vals (get state-after-play :players)))]
    (cond
      hands-empty
      (let [final-trick (if (empty? updated-trick) current-trick updated-trick)
            winner (when (seq final-trick)
                     (rules/resolve-trick final-trick (:trump state-after-play)))
            winner-team (when winner (get-in state-after-play [:players winner :team]))
            state-with-final-trick (cond-> state-after-play
                                     (and winner-team (seq final-trick))
                                     (update-in [:tricks-this-hand winner-team] inc)
                                     (and (seq final-trick))
                                     (update :completed-tricks (fn [tricks]
                                                                 (conj (or tricks []) final-trick))))
            team1-tricks (get-in state-with-final-trick [:tricks-this-hand 1])
            team2-tricks (get-in state-with-final-trick [:tricks-this-hand 2])
            tricks-this-hand {1 team1-tricks 2 team2-tricks}
            hand-index (:hand-index state-with-final-trick)
            bids-this-hand (filter #(= (:hand-index %) hand-index) (:bids state-with-final-trick))
            winning-bid (first (sort-by :value > (filter #(not= :pass (:bid-type %)) bids-this-hand)))
            bidding-player (:player winning-bid)
            bidding-team (get-in state-with-final-trick [:players bidding-player :team])
            bid-value (:value winning-bid)
            bidding-team-tricks (get tricks-this-hand bidding-team)
            other-team (if (= bidding-team 1) 2 1)
            other-team-tricks (get tricks-this-hand other-team)
            points-this-hand (if (>= bidding-team-tricks bid-value)
                               {bidding-team bidding-team-tricks other-team 0}
                               {bidding-team (- bidding-team-tricks bid-value)
                                other-team other-team-tricks})
            new-scores (merge-with + (:scores state-with-final-trick) points-this-hand)]
        (-> state-with-final-trick
            (assoc :phase :hand-complete)
            (update :tricks-per-hand conj tricks-this-hand)
            (update :points-per-hand conj points-this-hand)
            (assoc :scores new-scores)
            (assoc :tricks-this-hand {1 0, 2 0})))
      trick-complete?
      (let [winner (rules/resolve-trick updated-trick (:trump state))
            winner-team (get-in state [:players winner :team])]
        (-> state-after-play
            (update-in [:tricks-this-hand winner-team] inc)
            (assoc :current-trick [])
            (update :completed-tricks (fn [tricks]
                                        (conj (or tricks []) updated-trick)))
            (assoc :current-player winner)))
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
        players (vec (keys (:players state)))
        next-dealer (get players (mod (inc (.indexOf players current-dealer)) (count players)))
        initial-seed (:initial-seed state)
        new-seed (hash [initial-seed next-hand-index])]
    (-> (init-game new-seed next-hand-index bids trumps tricks-per-hand points-per-hand next-dealer initial-seed)
        (assoc :scores scores))))