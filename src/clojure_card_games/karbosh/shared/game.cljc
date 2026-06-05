(ns clojure-card-games.karbosh.shared.game
  (:require [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(def players [:player1 :player2 :player3 :player4 :player5 :player6])
(def target-score 52)

(defn teams []
  (zipmap players (cycle [1 2])))

(defn next-player [player]
  (get players (mod (inc (.indexOf players player)) (count players))))

(defn players-starting-at [player]
  (let [idx (.indexOf players player)
        idx (if (neg? idx) 0 idx)]
    (vec (take (count players) (drop idx (cycle players))))))

(defn next-player-in [order player]
  (get order (mod (inc (.indexOf order player)) (count order))))

(defn player-team [game player]
  (get-in game [:players player :team]))

(defn team-players [game team]
  (filterv #(= team (player-team game %)) players))

(defn partner-players [game player]
  (filterv #(not= player %) (team-players game (player-team game player))))

(defn lone-hand-players [game player]
  (let [team (player-team game player)]
    (filterv #(or (= player %)
                  (not= team (player-team game %)))
             players)))

(defn trick-players [game]
  (vec (or (seq (:active-players game)) players)))

(defn next-trick-player [game player]
  (next-player-in (trick-players game) player))

(defn current-bidder [game]
  (get-in game [:bidding-order (:current-bidder-index game)]))

(defn bids-this-hand [game]
  (filter #(= (:hand-index game) (:hand-index %)) (:bids game)))

(defn current-bid [game]
  (some-> (rules/winning-bid (bids-this-hand game))
          (dissoc :hand-index)))

(defn normalize-game [game]
  (dissoc game :current-bid))

(defn remove-first [x coll]
  (let [[before after] (split-with #(not= x %) coll)]
    (vec (concat before (rest after)))))

(defn player-hands [game]
  (into {}
        (map (fn [player]
               [player (get-in game [:players player :hand])])
             players)))

(defn- fail [message data]
  (throw (ex-info message data)))

(defn- require-phase [game phase event]
  (when-not (= phase (:phase game))
    (fail "Event is not valid in this phase"
          {:expected phase :actual (:phase game) :event event})))

(defn- require-current-player [game player event]
  (when-not (= player (:current-player game))
    (fail "Not this player's turn"
          {:expected (:current-player game) :actual player :event event})))

(defn- hand-summary [game points scores]
  {:hand-index (:hand-index game)
   :bid (current-bid game)
   :trump (:trump game)
   :initial-hands (:initial-hands game)
   :final-hands (player-hands game)
   :deals (:hand-deals game)
   :history (:history game)
   :completed-tricks (:completed-tricks game)
   :tricks (:tricks-this-hand game)
   :points points
   :scores-after scores})

(defn- complete-hand [game]
  (let [bid (current-bid game)
        tricks (:tricks-this-hand game)
        points (if bid
                 (rules/score-hand (:players game) bid tricks)
                 {1 0 2 0})
        scores (merge-with + (:scores game) points)
        winner (some (fn [[team score]]
                       (when (>= score target-score) team))
                     scores)]
    (cond-> game
      true
      (assoc :phase (if winner :game-over :hand-complete)
             :scores scores)
      true
      (update :hand-history (fnil conj []) (hand-summary game points scores))
      winner
      (assoc :winner winner)
      true
      (update :tricks-per-hand (fnil conj []) tricks)
      true
      (update :points-per-hand (fnil conj []) points))))

#?(:clj
   (defn with-deal [game seed]
     (let [deck (cards/shuffle-deck (cards/deck) seed)
           hands (zipmap players (cards/deal deck))
           default-teams (teams)]
       (-> game
           (assoc :deck deck
                  :initial-hands hands
                  :players (into {}
                                 (map (fn [[player hand]]
                                        [player {:hand hand
                                                 :team (or (player-team game player)
                                                           (get default-teams player))}])
                                      hands)))
           (update :hand-deals (fnil conj [])
                   {:hand-index (:hand-index game)
                    :seed seed
                    :hands hands})))))

#?(:clj
   (defn init-game
     ([] (init-game nil))
     ([seed]
      (with-deal
        {:phase :bidding
         :history []
         :scores {1 0 2 0}
         :bidding-order (players-starting-at :player1)
         :current-bidder-index 0
         :current-player (first players)
         :active-players players
         :completed-tricks []
         :bids []
         :trumps []
         :tricks-per-hand []
         :points-per-hand []
         :hand-history []
         :hand-deals []
         :hand-index 0
         :dealer :player1
         :tricks-this-hand {1 0 2 0}
         :initial-seed seed}
        seed))))

(defn apply-bid [game {:keys [player] :as event}]
  (require-phase game :bidding event)
  (when-not (= player (current-bidder game))
    (fail "Not this player's turn to bid"
          {:expected (current-bidder game) :actual player :event event}))
  (when-not (rules/valid-bid? event)
    (fail "Invalid bid" {:event event}))
  (let [event (select-keys event [:type :player :bid-type :value])
        hand-index (:hand-index game)
        updated-game (-> game
                         (update :history conj event)
                         (update :bids conj (assoc event :hand-index hand-index)))
        bids-this-hand (bids-this-hand updated-game)
        complete? (or (= :double-karbosh (:bid-type event))
                      (= (count bids-this-hand) (count (:bidding-order updated-game))))
        winning-bid (rules/winning-bid bids-this-hand)
        next-bidder-idx (mod (inc (:current-bidder-index updated-game))
                             (count (:bidding-order updated-game)))]
    (if complete?
      (if winning-bid
        (assoc updated-game
               :phase :trump-selection
               :current-player (:player winning-bid))
        (complete-hand updated-game))
      (assoc updated-game
             :current-bidder-index next-bidder-idx
             :current-player (get-in updated-game [:bidding-order next-bidder-idx])))))

(defn start-trick-playing [game player active-players]
  (assoc game
         :phase :trick-playing
         :current-trick []
         :active-players active-players
         :trick-leader player
         :current-player player))

(defn start-karbosh-discard [game player]
  (assoc game
         :phase :karbosh-discard
         :current-player player
         :discard-count 0))

(defn start-karbosh-donation [game player]
  (if-let [donor (first (:donation-order game))]
    (assoc game
           :phase :karbosh-donation
           :current-player donor)
    (start-trick-playing game player (:active-players game))))

(defn apply-trump-selection [game {:keys [player suit] :as event}]
  (require-phase game :trump-selection event)
  (require-current-player game player event)
  (when-not (contains? (set cards/suits) suit)
    (fail "Invalid trump suit" {:suit suit :event event}))
  (let [bid (current-bid game)
        game (-> game
                 (update :history conj event)
                 (assoc :trump suit)
                 (update :trumps conj suit))]
    (case (:bid-type bid)
      :karbosh
      (let [donors (partner-players game player)
            game (assoc game
                        :active-players (lone-hand-players game player)
                        :donation-order donors)]
        (start-karbosh-discard game player))

      :double-karbosh
      (start-trick-playing game player (lone-hand-players game player))

      (start-trick-playing game player players))))

(defn apply-donation [game {:keys [player card] :as event}]
  (require-phase game :karbosh-donation event)
  (require-current-player game player event)
  (let [bid (current-bid game)
        caller (:player bid)
        hand (get-in game [:players player :hand])]
    (when-not (some #(= card %) hand)
      (fail "Card is not in player's hand" {:player player :card card :event event}))
    (let [event (select-keys (assoc event :to caller) [:type :player :to :card])
          remaining-donors (vec (rest (:donation-order game)))
          updated-game (-> game
                           (update :history conj event)
                           (update :donations (fnil conj []) event)
                           (update-in [:players player :hand] #(remove-first card %))
                           (update-in [:players caller :hand] conj card))]
      (if-let [next-donor (first remaining-donors)]
        (assoc updated-game
               :donation-order remaining-donors
               :current-player next-donor)
        (start-trick-playing updated-game caller (:active-players updated-game))))))

(defn apply-discard [game {:keys [player card] :as event}]
  (require-phase game :karbosh-discard event)
  (require-current-player game player event)
  (let [hand (get-in game [:players player :hand])]
    (when-not (some #(= card %) hand)
      (fail "Card is not in player's hand" {:player player :card card :event event}))
    (let [discard-count (inc (or (:discard-count game) 0))
          updated-game (-> game
                           (update :history conj (select-keys event [:type :player :card]))
                           (update-in [:players player :hand] #(remove-first card %))
                           (assoc :discard-count discard-count))]
      (if (= 2 discard-count)
        (start-karbosh-donation updated-game player)
        updated-game))))

(defn apply-card [game {:keys [player card] :as event}]
  (require-phase game :trick-playing event)
  (require-current-player game player event)
  (let [current-trick (:current-trick game)
        hand (get-in game [:players player :hand])]
    (when-not (rules/legal-play? hand current-trick card (:trump game))
      (fail "Illegal card play" {:player player :card card :event event}))
    (let [updated-trick (conj current-trick {:player player :card card})
          complete? (= (count updated-trick) (count (trick-players game)))
          game-after-play (-> game
                              (update :history conj event)
                              (update-in [:players player :hand]
                                         #(remove-first card %)))
          hands-empty? (every? #(empty? (get-in game-after-play [:players % :hand]))
                               (trick-players game-after-play))]
      (if complete?
        (let [winner (rules/resolve-trick updated-trick (:trump game))
              winner-team (get-in game [:players winner :team])]
          (cond-> game-after-play
            true
            (update-in [:tricks-this-hand winner-team] inc)
            true
            (assoc :current-trick []
                   :current-player winner)
            true
            (update :completed-tricks conj updated-trick)
            hands-empty?
            complete-hand))
        (assoc game-after-play
               :current-trick updated-trick
               :current-player (next-trick-player game player))))))

#?(:clj
   (defn apply-new-hand [game]
     (require-phase game :hand-complete {:type :new-hand})
     (let [next-hand-index (inc (:hand-index game))
           current-dealer (:dealer game)
           next-dealer (next-player current-dealer)
           initial-seed (:initial-seed game)
           new-seed (hash [initial-seed next-hand-index])]
       (-> game
           (dissoc :trump
                   :current-trick
                   :trick-leader
                   :winner
                   :donation-order
                   :discard-count
                   :donations)
           (assoc :hand-index next-hand-index
                  :phase :bidding
                  :dealer next-dealer
                  :history []
                  :bidding-order (players-starting-at next-dealer)
                  :current-bidder-index 0
                  :current-player next-dealer
                  :active-players players
                  :completed-tricks []
                  :tricks-this-hand {1 0 2 0}
                  :hand-deals []
                  :initial-seed initial-seed)
           (with-deal new-seed)))))

#?(:clj
   (defn apply-new-game [game {:keys [seed] :as event}]
     (require-phase game :game-over event)
     (when-not (contains? event :seed)
       (fail "New game needs an event seed" {:event event}))
     (init-game seed)))

#?(:clj
   (defn reshuffle-hand [game {:keys [seed] :as event}]
     (require-phase game :bidding event)
     (when (seq (bids-this-hand game))
       (fail "Cannot reshuffle after bidding has started" {:event event}))
     (when-not (contains? event :seed)
       (fail "Reshuffle needs an event seed" {:event event}))
     (with-deal game seed)))

(defn apply-event [game event]
  (let [game (normalize-game game)]
    (case (:type event)
      :bid (apply-bid game event)
      :trump-selection (apply-trump-selection game event)
      :donate-card (apply-donation game event)
      :discard-card (apply-discard game event)
      :play-card (apply-card game event)
      #?(:clj :new-hand :cljs ::new-hand) #?(:clj (apply-new-hand game)
                                             :cljs (fail "New hands are started by the server" {:event event}))
      #?(:clj :new-game :cljs ::new-game) #?(:clj (apply-new-game game event)
                                             :cljs (fail "New games are started by the server" {:event event}))
      #?(:clj :reshuffle-hand :cljs ::reshuffle-hand) #?(:clj (reshuffle-hand game event)
                                                         :cljs (fail "Hands are reshuffled by the server" {:event event}))
      (fail "Unknown event type" {:event event}))))

(defn debug-view [game]
  {:can-reshuffle? (and (= :bidding (:phase game))
                        (empty? (bids-this-hand game)))
   :hands (player-hands game)
   :initial-hands (:initial-hands game)
   :deals (:hand-deals game)
   :history (:history game)
   :hand-history (:hand-history game)})

(defn public-view [game seats player]
  {:room-phase (:phase game)
   :phase (:phase game)
   :scores (:scores game)
   :current-bid (current-bid game)
   :bids-this-hand (mapv #(select-keys % [:player :bid-type :value])
                         (bids-this-hand game))
   :current-player (:current-player game)
   :dealer (:dealer game)
   :trump (:trump game)
   :current-trick (:current-trick game)
   :completed-tricks (:completed-tricks game)
   :tricks-this-hand (:tricks-this-hand game)
   :active-players (:active-players game)
   :donation-order (:donation-order game)
   :discard-count (:discard-count game)
   :hand-index (:hand-index game)
   :winner (:winner game)
   :you player
   :hand (get-in game [:players player :hand])
   :players (mapv (fn [p]
                    (let [seat (get seats p)]
                      {:id p
                       :team (get-in game [:players p :team])
                       :name (:name seat)
                       :connected? (:connected? seat)
                       :bot? (:bot? seat)
                       :persona (:persona seat)
                       :active? (contains? (set (trick-players game)) p)
                       :hand-count (count (get-in game [:players p :hand]))}))
                  players)})

(defn admin-view [game seats]
  (assoc (dissoc (public-view game seats nil) :hand :you)
         :debug (debug-view game)))
