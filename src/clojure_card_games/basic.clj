(ns clojure-card-games.basic
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]))

;; Card and Deck functions
(defn cards []
  "Karbosh cards."
  [9 10 :J :Q :K :A])

(defn suits []
  "Karbosh suits."
  [:♥ :♠ :♦ :♣])

(def players [:player1 :player2 :player3 :player4 :player5 :player6])

(defn karbosh-deck []
  "Create karbosh deck. Two decks from 9 to Ace of each suit."
  (vec (mapcat (partial repeat 2)
               (combo/cartesian-product (cards) (suits)))))

(defn init-deck []
  "Shuffle the deck."
  (shuffle (karbosh-deck)))

(defn deal-hands [deck]
  "Deal hands with 8 cards each from the deck."
  (mapv vec (partition 8 deck)))

(defn assign-teams []
  "Assign alternating teams to players."
  (zipmap players (cycle [1 2])))

;; Game State Management
(defn init-game []
  "Initialize a new game with proper phase tracking and history."
  (let [deck (init-deck)
        hands (deal-hands deck)
        teams (assign-teams)]
    {:phase :bidding
     :history []
     :deck deck
     :players (zipmap players
                      (map (fn [hand player]
                             {:hand hand
                              :team (teams player)})
                           hands
                           players))
     :scores {1 0, 2 0}
     :current-bid nil
     :next-to-bid (first players)
     :current-player (first players)
     :completed-tricks []
     :legal-moves #{:bid :pass :karbosh :double-karbosh}}))

;; Card Play Validation
(defn legal-play?
  "Check if a card play is legal given the current game state."
  [game-state player card]
  (let [current-trick (get-in game-state [:current-trick] [])
        player-hand (get-in game-state [:players player :hand])
        lead-suit (when (seq current-trick)
                    (second (get-in current-trick [0 :card])))]
    (and (contains? (set player-hand) card)
         (or (empty? current-trick)
             (nil? lead-suit)
             (= (second card) lead-suit)
             (not-any? #(= lead-suit (second %)) player-hand)))))

;; Trick Resolution
(defn resolve-trick
  "Determine the winner of a trick based on the trump suit and lead suit.
   Right bower (Jack of trump) is highest
   Left bower (Jack of same color suit) is second highest"
  [trick trump]
  (let [lead-suit (-> trick first :card second)
        rank-value {9 0, 10 1, :Q 2, :K 3, :A 4}  ; Note: J handled separately for bower rules
        right-bower? (fn [[rank suit]]
                       (and (= rank :J)
                            (= suit trump)))
        left-bower? (fn [[rank suit]]
                      (and (= rank :J)
                           (case trump
                             :♥ (= suit :♦)
                             :♦ (= suit :♥)
                             :♠ (= suit :♣)
                             :♣ (= suit :♠)
                             false)))]
    (->> trick
         (sort-by (fn [{:keys [card]}]
                    (let [[rank suit] card
                          base-rank (get rank-value rank 0)]
                      (cond
                        (right-bower? card) 1000  ; Right bower always wins
                        (left-bower? card) 999    ; Left bower second highest
                        (= suit trump) (+ 100 base-rank)  ; Other trump
                        (= suit lead-suit) base-rank      ; Following suit
                        :else -1)))                       ; Off-suit
                  >)
         first
         :player)))

(defn resolve-scoring
  "Resolve final scores for a hand based on bid type and tricks taken"
  [game-state]
  (let [{:keys [type value player]} (:current-bid game-state)
        bidder-team (get-in game-state [:players player :team])
        other-team (if (= bidder-team 1) 2 1)
        team1-tricks (get-in game-state [:scores 1])
        team2-tricks (get-in game-state [:scores 2])
        bidding-team-tricks (if (= bidder-team 1) team1-tricks team2-tricks)
        defending-team-tricks (if (= bidder-team 1) team2-tricks team1-tricks)]

    (case type
      :karbosh
      (if (>= bidding-team-tricks defending-team-tricks)
        {bidder-team 15, other-team 0}
        {bidder-team -15, other-team defending-team-tricks})

      :double-karbosh
      (if (>= bidding-team-tricks defending-team-tricks)
        {bidder-team 15, other-team 0}
        {bidder-team -15, other-team defending-team-tricks})

      ;; Regular bid
      (if (>= bidding-team-tricks value)
        {bidder-team bidding-team-tricks, other-team 0}
        {bidder-team (- value), other-team defending-team-tricks}))))

;; Event Handling
(defmulti apply-event
          "Handle different types of game events."
          (fn [game event] (:type event)))

(defmethod apply-event :bid
  [game {:keys [player bid-type value] :as event}]
  (let [next-player (get players
                         (mod (inc (.indexOf players player))
                              (count players)))
        history-with-current (conj (:history game) event)
        ;; Find last non-pass bid
        last-bid (->> history-with-current
                      (filter #(and (= (:type %) :bid)
                                    (not= :pass (:bid-type %))))
                      last)
        ;; Check if we've completed bidding
        bidding-complete? (or
                            (= bid-type :double-karbosh)  ; Double karbosh immediately ends bidding
                            (and
                              (= player (last players))    ; Current player is last player
                              (or (not= :pass bid-type)    ; Either made a non-pass bid
                                  (and last-bid            ; Or has passed and we have a previous bid
                                       (= :pass bid-type)))))
        ;; Double karbosh overrides previous bidder
        winning-bidder (cond
                         (= bid-type :double-karbosh) player
                         (and bidding-complete? last-bid) (:player last-bid)
                         :else player)]
    (-> game
        (update :history conj event)
        (assoc :current-bid {:player player
                             :type bid-type
                             :value value})
        (assoc :phase (cond
                        (= bid-type :karbosh) :card-exchange
                        (= bid-type :double-karbosh) :trump-selection
                        bidding-complete? :trump-selection
                        :else :bidding))
        (assoc :next-to-bid next-player)
        (cond-> (or bidding-complete?
                    (#{:karbosh :double-karbosh} bid-type))
                (assoc :current-player winning-bidder)))))

(defmethod apply-event :trump-selection
  [game {:keys [player suit] :as event}]
  (let [current-player (:current-player game)]
    (when (not= player current-player)
      (throw (ex-info "Only the current player can select trump"
                      {:expected current-player
                       :actual player})))
    (when-not (contains? (set (suits)) suit)
      (throw (ex-info "Invalid suit selection"
                      {:suit suit
                       :valid-suits (suits)})))
    (-> game
        (update :history conj event)
        (assoc :trump suit)
        (assoc :phase :trick-playing)
        (assoc :current-trick [])
        ;; Keep same player as leader for first trick
        (assoc :trick-leader player))))

(defmethod apply-event :play-card
  [game {:keys [player card] :as event}]
  (let [current-trick (:current-trick game)
        player-hand (get-in game [:players player :hand])
        next-player (get players
                         (mod (inc (.indexOf players player))
                              (count players)))]

    ;; Validations...

    (let [updated-trick (conj current-trick {:player player :card card})
          trick-complete? (= (count updated-trick) 6)]
      (if trick-complete?
        (let [winner (resolve-trick updated-trick (:trump game))
              winner-team (get-in game [:players winner :team])
              new-trick-count (inc (:trick-count game 0))
              hand-complete? (= new-trick-count 8)]
          (cond-> game
                  true (update :history conj event)
                  true (update-in [:players player :hand] #(remove #{card} %))
                  true (update-in [:scores winner-team] inc)
                  true (assoc :current-trick [])
                  true (update :completed-tricks (fn [tricks]
                                                   (conj (or tricks []) updated-trick)))
                  true (assoc :current-player winner)
                  true (update :trick-count (fnil inc 0))

                  hand-complete?
                  (-> (assoc :final-scores (resolve-scoring game))
                      (assoc :phase :hand-complete))))
        ;; Regular trick in progress...
        (-> game
            (update :history conj event)
            (update-in [:players player :hand] #(remove #{card} %))
            (assoc :current-trick updated-trick)
            (assoc :current-player next-player))))))

(defmethod apply-event :default
  [game event]
  (throw (ex-info "Unknown event type"
                  {:event event
                   :valid-types #{:bid :trump-selection :card-exchange :play-card}})))

;; Helper Functions
(defn valid-bid?
  "Validate if a bid value is legal."
  [value]
  (and (number? value)
       (<= 1 value 8)))

(defn get-valid-moves
  "Get the set of valid moves for a player in the current game state."
  [game player]
  (case (:phase game)
    :bidding #{:bid :pass :karbosh :double-karbosh}
    :trump-selection #{:select-trump}
    :card-exchange #{:exchange-cards}
    :trick-playing (set (filter #(legal-play? game player %)
                                (get-in game [:current-state :players player :hand])))))

;; Export internals for testing
(def internal-states
  {:deck karbosh-deck
   :deal-hands deal-hands
   :init-game init-game
   :legal-play? legal-play?
   :resolve-trick resolve-trick
   :valid-bid? valid-bid?})