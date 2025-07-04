(ns clojure-card-games.basic
  (:require [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]))

;; ============================================================================
;; Type Definitions (for documentation)
;; ============================================================================

;; Card = [Rank Suit]
;; Rank = (or :A :K :Q :J 10 9)
;; Suit = (or :♥ :♠ :♦ :♣)
;; Hand = [Card]
;; Player = (or :player1 :player2 :player3 :player4 :player5 :player6)
;; Team = (or 1 2)
;; BidType = (or :pass :bid :karbosh :double-karbosh)
;; GamePhase = (or :bidding :trump-selection :trick-playing :hand-complete)
;; Trick = [{:player Player, :card Card}]

;; ============================================================================
;; Constants and Configuration
;; ============================================================================

(def ^:private PLAYERS [:player1 :player2 :player3 :player4 :player5 :player6])
(def ^:private CARDS_PER_HAND 8)
(def ^:private MIN_BID 1)
(def ^:private MAX_BID 8)
(def ^:private KARBOSH_POINTS 15)

;; Unicode Playing Card Constants
;; The Unicode Playing Cards block (U+1F0A0 to U+1F0FF) contains all playing card symbols.
;; Each suit has its own 16-card block, with ranks ordered from Ace to King.
(def ^:private SUIT-BASE
  {:♠ 0x1F0A0   ;; Spades block (U+1F0A0 to U+1F0AF)
   :♥ 0x1F0B0   ;; Hearts block (U+1F0B0 to U+1F0BF)
   :♦ 0x1F0C0   ;; Diamonds block (U+1F0C0 to U+1F0CF)
   :♣ 0x1F0D0}) ;; Clubs block (U+1F0D0 to U+1F0DF)

;; Rank offsets within each suit block.
;; Each rank has a specific offset from its suit's base code point.
;; Example: For Hearts (base 0x1F0B0):
;;   - Ace (A) = 0x1F0B0 + 1 = 0x1F0B1 (🂱)
;;   - King (K) = 0x1F0B0 + 13 = 0x1F0BD (🂽)
(def ^:private RANK-OFFSET
  {1 1, 2 2, 3 3, 4 4, 5 5, 6 6, 7 7, 8 8, 9 9, 10 10  ; numeric keys
   :A 1, :2 2, :3 3, :4 4, :5 5, :6 6, :7 7, :8 8, :9 9, :10 10
   :J 11, :Q 12, :K 13})

(def ^:private RANK-VALUE
  {9 0, 10 1, :Q 2, :K 3, :A 4})  ; Used for trick resolution

;; Card Value Configuration
;; Each card has a base value, and its effective value is determined by context
(def ^:private CARD-BASE-VALUES
  {:A 8, :K 7, :Q 6, :J 5, 10 4, 9 3, 8 2, 7 1})

;; Card Value Multipliers
;; These determine how much to multiply the base value based on context
(def ^:private VALUE-MULTIPLIERS
  {:trump 100    ; Trump cards are worth 100x their base value
   :lead  10     ; Lead suit cards are worth 10x their base value
   :off   1})    ; Off-suit cards are worth their base value

;; Card Value Bonuses
;; Special cards get fixed bonuses regardless of their base value
(def ^:private VALUE-BONUSES
  {:right-bower 1000  ; Right bower is always highest
   :left-bower  900}) ; Left bower is second highest

;; ============================================================================
;; Core Card and Deck Functions
;; ============================================================================

(defn cards
  "Returns the set of valid card ranks for Karbosh.
   
   Returns: [Rank]"
  []
  [9 10 :J :Q :K :A])

(defn suits
  "Returns the set of valid card suits.
   
   Returns: [Suit]"
  []
  [:♥ :♠ :♦ :♣])

(defn- code-point [s]
  "Converts a string to its Unicode code point."
  (Character/codePointAt s 0))

(defn- code-point-to-string [cp]
  "Converts a Unicode code point to a string."
  (String. (Character/toChars cp)))

(defn unicode-card
  "Converts a card (suit and rank) to its Unicode playing card symbol.
   
   The function uses the Unicode Playing Cards block (U+1F0A0 to U+1F0FF) to generate
   the appropriate card symbol. Each suit has its own 16-card block, with ranks ordered
   from Ace to King.
   
   Examples:
     (unicode-card :♥ :Q) → \"🂺\"  ; Queen of Hearts
     (unicode-card :♠ :A) → \"🂡\"  ; Ace of Spades
     (unicode-card :♣ :K) → \"🃞\"  ; King of Clubs
   
   Args:
     suit - Suit - The card's suit (:♥ :♠ :♦ :♣)
     rank - Rank - The card's rank (:A :K :Q :J 10 9)
   
   Returns: string - The Unicode playing card symbol
   
   Throws:
     ex-info - If suit or rank is invalid"
  [suit rank]
  (let [base (SUIT-BASE suit)
        offset (RANK-OFFSET rank)]
    (when (nil? base)
      (throw (ex-info "Invalid suit" 
                     {:suit suit 
                      :valid-suits (keys SUIT-BASE)})))
    (when (nil? offset)
      (throw (ex-info "Invalid rank" 
                     {:rank rank 
                      :valid-ranks (keys RANK-OFFSET)})))
    (code-point-to-string (+ base offset))))

(defn karbosh-deck
  "Creates a Karbosh deck with two copies of each card from 9 to Ace of each suit.
   
   Returns: [Card]"
  []
  (vec (mapcat (partial repeat 2)
               (combo/cartesian-product (cards) (suits)))))

(defn- seeded-shuffle [coll rng]
  "Shuffles a collection using the provided random number generator."
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al rng)
    (vec al)))

(defn init-deck
  "Creates and shuffles a new Karbosh deck.
   Optionally accepts a seed for reproducible shuffling.
   
   Args:
     seed - (optional) Long - Seed for reproducible shuffling
   
   Returns: [Card]"
  ([] (init-deck nil))
  ([^Long seed]
   (let [deck (karbosh-deck)
         rng (if seed
               (java.util.Random. seed)
               (let [random-seed (.nextLong (java.util.Random.))
                     rng (java.util.Random. random-seed)]
                 (println "\nUsing random seed:" random-seed)
                 rng))]
     (seeded-shuffle deck rng))))

(defn deal-hands
  "Deals hands with 8 cards each from the deck.
   
   Args:
     deck - [Card] - The deck to deal from
   
   Returns: [[Card]] - Vector of player hands"
  [^clojure.lang.PersistentVector deck]
  (mapv vec (partition CARDS_PER_HAND deck)))

(defn assign-teams []
  "Assigns alternating teams to players."
  (zipmap PLAYERS (cycle [1 2])))

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defn right-bower?
  "Checks if a card is the right bower (Jack of trump).
   
   Args:
     card - Card - The card to check
     trump - Suit - The trump suit
   
   Returns: boolean"
  [[rank suit] trump]
  (and (= rank :J)
       (= suit trump)))

(defn left-bower?
  "Checks if a card is the left bower (Jack of same color suit).
   
   Args:
     card - Card - The card to check
     trump - Suit - The trump suit
   
   Returns: boolean"
  [[rank suit] trump]
  (and (= rank :J)
       (case trump
         :♥ (= suit :♦)
         :♦ (= suit :♥)
         :♠ (= suit :♣)
         :♣ (= suit :♠)
         false)))

(defn- card-base-value
  "Returns the base value of a card rank.
   
   Args:
     rank - Rank - The card's rank
   
   Returns: number - The base value of the rank"
  [rank]
  (get CARD-BASE-VALUES rank 0))

(defn effective-suit
  "Returns the effective suit of a card given the trump suit.
   This handles the left bower case by returning the trump suit.
   
   Args:
     card - Card - The card to check
     trump - Suit - The trump suit
   
   Returns: Suit - The effective suit of the card"
  [[rank suit] trump]
  (if (and (= rank :J)
           (= suit (case trump
                    :♥ :♦
                    :♦ :♥
                    :♠ :♣
                    :♣ :♠)))
    trump
    suit))

(defn- cards-of-suit
  "Returns all cards in a hand that have the given effective suit.
   
   Args:
     hand - [Card] - The hand to check
     suit - Suit - The suit to look for
     trump - Suit - The trump suit
   
   Returns: [Card] - Cards with the given effective suit"
  [hand suit trump]
  (filter #(= suit (effective-suit % trump)) hand))

(defn has-suit?
  "Checks if a hand contains any cards of the given suit.
   
   Args:
     hand - [Card] - The hand to check
     suit - Suit - The suit to look for
     trump - Suit - The trump suit
   
   Returns: boolean"
  [hand suit trump]
  (seq (cards-of-suit hand suit trump)))

(defn card-value
  "Returns the value of a card in the current trick context.
   
   Args:
     card - Card - The card to evaluate
     trump - Suit - The trump suit
     lead-suit - Suit - The suit that led the trick
   
   Returns: number - Card's value in the current context"
  [[rank suit] trump lead-suit]
  (let [effective-suit (effective-suit [rank suit] trump)
        base-value (card-base-value rank)]
    (cond
      ;; Right bower check
      (and (= rank :J) (= suit trump))
      (:right-bower VALUE-BONUSES)
      
      ;; Left bower check
      (and (= rank :J) (= effective-suit trump))
      (:left-bower VALUE-BONUSES)
      
      ;; Trump cards
      (= effective-suit trump)
      (* base-value (:trump VALUE-MULTIPLIERS))
      
      ;; Lead suit cards
      (and lead-suit (= effective-suit lead-suit))
      (* base-value (:lead VALUE-MULTIPLIERS))
      
      ;; Off-suit cards
      :else
      (* base-value (:off VALUE-MULTIPLIERS)))))

(defn- evaluate-hand-strength [hand trump]
  "Evaluates the strength of a hand with a given trump suit.
   Returns a score based on the sum of card values and number of trump cards."
  (let [sorted-hand (sort-by #(card-value % trump nil) > hand)
        trump-cards (filter #(= (second %) trump) hand)
        bower-cards (filter #(or (right-bower? % trump)
                               (left-bower? % trump)) hand)
        high-cards (filter #(>= (get RANK-VALUE (first %) 0) 2) hand)]  ; A, K, Q
    (+ (* 100 (count trump-cards))     ; Trump cards are worth 100 each
       (* 200 (count bower-cards))     ; Bowers are worth 200 each
       (* 50 (count high-cards)))))    ; High cards are worth 50 each

(defn- sort-hand-by-trump [hand trump]
  "Sorts a hand of cards based on their values with the given trump suit.
   Groups cards by suit, with trump first, then other suits by strength.
   Left bower is treated as part of the trump suit."
  (let [trump-cards (filter #(or (= (second %) trump)
                               (left-bower? % trump)) hand)
        other-suits (remove #{trump} (distinct (map second hand)))
        suit-strengths (map (fn [suit]
                            {:suit suit
                             :strength (evaluate-hand-strength 
                                       (filter #(= (second %) suit) hand)
                                       trump)})
                          other-suits)
        sorted-suits (->> suit-strengths
                         (sort-by :strength >)
                         (map :suit))
        all-suits (cons trump sorted-suits)
        suit-groups (map (fn [suit]
                          (if (= suit trump)
                            ;; For trump suit, include left bower and sort by card value
                            (->> hand
                                 (filter #(or (= (second %) trump)
                                            (left-bower? % trump)))
                                 (sort-by #(card-value % trump nil) >))
                            ;; For other suits, exclude left bower
                            (->> hand
                                 (filter #(and (= (second %) suit)
                                             (not (left-bower? % trump))))
                                 (sort-by #(card-value % trump nil) >))))
                        all-suits)]
    (apply concat suit-groups)))

(defn find-best-trump [hand]
  "Finds the best trump suit for a given hand.
   Returns a map with the best trump and the sorted hand."
  (let [trumps [:♥ :♠ :♦ :♣]
        evaluations (map (fn [trump]
                          {:trump trump
                           :strength (evaluate-hand-strength hand trump)
                           :sorted-hand (sort-hand-by-trump hand trump)})
                        trumps)
        best-eval (apply max-key :strength evaluations)]
    {:best-trump (:trump best-eval)
     :strength (:strength best-eval)
     :sorted-hand (:sorted-hand best-eval)}))

(defn sort-hand
  "Sorts a hand by card value, considering trump if provided.
   Returns a map with :sorted-hand and :analysis."
  ([hand]
   (sort-hand hand nil))
  ([hand trump]
   (let [analysis (reduce (fn [acc card]
                           (let [suit (second card)
                                 is-trump (= suit trump)
                                 is-left-bower (and is-trump
                                                  (= (first card) :J)
                                                  (= suit (case trump
                                                           :♥ :♦
                                                           :♦ :♥
                                                           :♠ :♣
                                                           :♣ :♠)))
                                 value (card-value card trump nil)]
                             (-> acc
                                 (update :trump-count #(if is-trump (inc %) %))
                                 (update :left-bower #(or % is-left-bower))
                                 (update :sorted-hand conj [card value]))))
                         {:trump-count 0
                          :left-bower false
                          :sorted-hand []}
                         hand)
         sorted-hand (->> (:sorted-hand analysis)
                         (sort-by second >)
                         (map first))]
     {:sorted-hand sorted-hand
      :analysis (dissoc analysis :sorted-hand)})))

(defn- next-player [current-player]
  "Gets the next player in sequence."
  (get PLAYERS
       (mod (inc (.indexOf PLAYERS current-player))
            (count PLAYERS))))

(defn- next-phase [bid-type bidding-complete?]
  "Determines the next game phase based on the bid type and completion status."
  (cond
    (= bid-type :double-karbosh) :hand-complete
    bidding-complete? :trump-selection
    :else :bidding))

(defn all-hands-empty?
  "Returns true if all players have empty hands."
  [game]
  (every? (comp empty? :hand) (vals (:players game))))

;; ============================================================================
;; Game State Management
;; ============================================================================

(defn- create-player-state [hand player team]
  "Creates the initial state for a player."
  {:hand hand
   :team team})

(defn init-game
  "Initializes a new game with proper phase tracking and history.
   Optionally accepts a seed for reproducible deck shuffling."
  ([] (init-game nil 0 [] [] [] [] :player1))
  ([seed] (init-game seed 0 [] [] [] [] :player1))
  ([seed hand-index bids trumps tricks-per-hand points-per-hand dealer]
   (let [deck (init-deck seed)
         hands (deal-hands deck)
         teams (assign-teams)]
     {:phase :bidding
      :history []
      :deck deck
      :players (zipmap PLAYERS
                       (map create-player-state
                            hands
                            PLAYERS
                            (vals teams)))
      :scores {1 0, 2 0}
      :current-bid nil
      :bidding-order (vec PLAYERS)  ; Track the order of bidding
      :current-bidder-index 0       ; Index into bidding-order
      :current-player (first PLAYERS)
      :completed-tricks []
      :legal-moves #{:bid :pass :karbosh :double-karbosh}
      :bids bids
      :trumps trumps
      :tricks-per-hand tricks-per-hand
      :points-per-hand points-per-hand
      :hand-index hand-index
      :dealer dealer
      :tricks-this-hand {1 0, 2 0}})))

(defn get-current-bidder [game]
  "Gets the player who should bid next."
  (get (:bidding-order game) (:current-bidder-index game)))

(defn next-bidder [game]
  "Advances to the next bidder in sequence."
  (let [next-index (mod (inc (:current-bidder-index game)) 
                       (count (:bidding-order game)))]
    (-> game
        (assoc :current-bidder-index next-index)
        (assoc :current-player (get (:bidding-order game) next-index)))))

(defn- bidding-complete? [game]
  "Determines if the bidding phase is complete based on game state."
  (let [last-event (last (:history game))]
    (or (= (:bid-type last-event) :double-karbosh)  ; Double karbosh immediately ends bidding
        (= (count (:history game)) (count (:bidding-order game))))))  ; All players have bid once

(defn- winning-bidder [game]
  "Determines the winning bidder after bidding is complete."
  (let [last-event (last (:history game))
        bids (->> (:history game)
                  (filter #(and (= (:type %) :bid)
                              (not= :pass (:bid-type %))))
                  (sort-by :value >))  ; Sort by bid value descending
        highest-bid (first bids)]
    (cond
      (= (:bid-type last-event) :double-karbosh) (:player last-event)
      highest-bid (:player highest-bid)
      :else (:player last-event))))

(defn- calculate-team-scores [game-state]
  (let [{:keys [type value player]} (:current-bid game-state)
        bidder-team (get-in game-state [:players player :team])
        other-team (if (= bidder-team 1) 2 1)
        team1-tricks (get-in game-state [:tricks-this-hand 1])
        team2-tricks (get-in game-state [:tricks-this-hand 2])
        bidding-team-tricks (if (= bidder-team 1) team1-tricks team2-tricks)
        defending-team-tricks (if (= bidder-team 1) team2-tricks team1-tricks)]
    (cond
      (or (nil? type) (= type :pass) (nil? value))
      {1 0, 2 0}

      (#{:karbosh :double-karbosh} type)
      (if (>= bidding-team-tricks defending-team-tricks)
        {bidder-team KARBOSH_POINTS, other-team 0}
        {bidder-team (- KARBOSH_POINTS), other-team defending-team-tricks})

      :else
      (if (>= bidding-team-tricks value)
        {bidder-team bidding-team-tricks, other-team 0}
        {bidder-team (- value), other-team defending-team-tricks}))))

(defn legal-play?
  "Checks if a card play is legal given the current game state.
   A player must follow suit if they can, and can only play off-suit if they cannot follow suit.
   
   Args:
     game-state - map - The current game state
     player - Player - The player making the play
     card - Card - The card to play
   
   Returns: boolean"
  [game-state player card]
  (let [current-trick (get-in game-state [:current-trick] [])
        player-hand (get-in game-state [:players player :hand])
        trump (:trump game-state)
        lead-suit (when (seq current-trick)
                   (second (get-in current-trick [0 :card])))
        card-suit (effective-suit card trump)]
    (and (contains? (set player-hand) card)  ; Card must be in player's hand
         (or (empty? current-trick)          ; First card of trick
             (nil? lead-suit)                ; No lead suit (shouldn't happen)
             (= card-suit lead-suit)         ; Following suit
             (not (has-suit? player-hand lead-suit trump))))))  ; Can't follow suit

(defn resolve-trick
  "Determines the winner of a trick based on the trump suit and lead suit."
  [trick trump]
  (let [lead-suit (-> trick first :card second)]
    (->> trick
         (sort-by (fn [{:keys [card]}]
                    (card-value card trump lead-suit))
                  >)
         first
         :player)))

;; ============================================================================
;; Event Handling
;; ============================================================================

(defmulti apply-event
  "Applies a game event to the current game state.
   Events are maps with at least a :type key."
  (fn [game event] (:type event)))

(defmethod apply-event :bid
  [game {:keys [player bid-type value] :as event}]
  (let [current-bidder (get-current-bidder game)
        _ (when (not= player current-bidder)
            (throw (ex-info "Not this player's turn to bid"
                           {:expected current-bidder
                            :actual player})))
        hand-index (:hand-index game)
        updated-game (-> game
                        (update :history conj event)
                        (update :bids conj (assoc event :hand-index hand-index)))
        bidding-complete? (bidding-complete? updated-game)
        winning-bidder (winning-bidder updated-game)
        bids (->> (:history updated-game)
                  (filter #(and (= (:type %) :bid)
                                (not= :pass (:bid-type %))))
                  (sort-by :value >))
        winning-bid (first bids)]
    (-> updated-game
        (assoc :phase (next-phase bid-type bidding-complete?))
        (cond-> (not bidding-complete?)
                (assoc :current-bid event))
        (cond-> bidding-complete?
                (assoc :current-bid winning-bid))
        (cond-> (not bidding-complete?)  ; Only advance to next bidder if bidding is not complete
                next-bidder)
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
        (update :trumps conj suit)
        (assoc :phase :trick-playing)
        (assoc :current-trick [])
        ;; Keep same player as leader for first trick
        (assoc :trick-leader player))))

(defmethod apply-event :play-card
  [game {:keys [player card] :as event}]
  (let [current-trick (:current-trick game)
        player-hand (get-in game [:players player :hand])
        next-player (next-player player)
        updated-trick (conj current-trick {:player player :card card})
        trick-complete? (= (count updated-trick) 6)
        game-after-play (-> game
                            (update :history conj event)
                            (update-in [:players player :hand]
                                       #(let [idx (.indexOf % card)]
                                          (vec (concat (subvec % 0 idx)
                                                      (subvec % (inc idx)))))))
        hands-empty (all-hands-empty? game-after-play)]
    (cond
      ;; If all hands are empty, resolve the last trick (even if not full), end hand
      hands-empty
      (let [final-trick (if (empty? updated-trick) current-trick updated-trick)
            winner (when (seq final-trick)
                     (resolve-trick final-trick (:trump game-after-play)))
            winner-team (when winner (get-in game-after-play [:players winner :team]))
            game-with-final-trick (cond-> game-after-play
                                    (and winner-team (seq final-trick))
                                    (update-in [:scores winner-team] #(+ (or % 0) 1))
                                    (and winner-team (seq final-trick))
                                    (update-in [:tricks-this-hand winner-team] inc)
                                    (and (seq final-trick))
                                    (update :completed-tricks (fn [tricks]
                                                                (conj (or tricks []) final-trick))))
            team1-tricks (get-in game-with-final-trick [:tricks-this-hand 1])
            team2-tricks (get-in game-with-final-trick [:tricks-this-hand 2])
            tricks-this-hand {1 team1-tricks 2 team2-tricks}
            points-this-hand (calculate-team-scores (assoc game-with-final-trick :tricks-this-hand tricks-this-hand))
            new-scores (merge-with + (:scores game-with-final-trick) points-this-hand)
            game-over? (or (>= (new-scores 1) 52) (>= (new-scores 2) 52))]
        (if game-over?
          (-> game-with-final-trick
              (assoc :final-scores points-this-hand)
              (assoc :phase :game-over)
              (update :tricks-per-hand conj tricks-this-hand)
              (update :points-per-hand conj points-this-hand)
              (assoc :scores new-scores)
              (assoc :winner (if (>= (new-scores 1) 52) 1 2))
              (assoc :tricks-this-hand {1 0, 2 0}))
          (-> game-with-final-trick
              (assoc :final-scores points-this-hand)
              (assoc :phase :hand-complete)
              (update :tricks-per-hand conj tricks-this-hand)
              (update :points-per-hand conj points-this-hand)
              (assoc :scores new-scores)
              (assoc :tricks-this-hand {1 0, 2 0}))))
      ;; If trick is complete, normal trick logic
      trick-complete?
      (let [winner (resolve-trick updated-trick (:trump game))
            winner-team (get-in game [:players winner :team])
            new-trick-count (inc (:trick-count game 0))]
        (-> game-after-play
            (update-in [:scores winner-team] inc)
            (update-in [:tricks-this-hand winner-team] inc)
            (assoc :current-trick [])
            (update :completed-tricks (fn [tricks]
                                        (conj (or tricks []) updated-trick)))
            (assoc :current-player winner)
            (update :trick-count (fnil inc 0))))
      ;; Otherwise, trick in progress
      :else
      (-> game-after-play
          (assoc :current-trick updated-trick)
          (assoc :current-player next-player)))))

(defmethod apply-event :new-hand
  [game _]
  (let [next-hand-index (inc (:hand-index game))
        bids (:bids game)
        trumps (:trumps game)
        tricks-per-hand (:tricks-per-hand game)
        points-per-hand (:points-per-hand game)
        scores (:scores game)
        current-dealer (:dealer game)
        next-dealer (next-player current-dealer)]
    (init-game nil next-hand-index bids trumps tricks-per-hand points-per-hand next-dealer)))

(defmethod apply-event :default
  [game event]
  (throw (ex-info "Unknown event type"
                 {:event event
                  :valid-types #{:bid :trump-selection :card-exchange :play-card}})))

;; ============================================================================
;; Public API
;; ============================================================================

(defn valid-bid?
  "Validates if a bid value is legal."
  [value]
  (and (number? value)
       (<= MIN_BID value MAX_BID)))

(defn get-valid-moves
  "Gets the set of valid moves for a player in the current game state."
  [game player]
  (case (:phase game)
    :bidding #{:bid :pass :karbosh :double-karbosh}
    :trump-selection #{:select-trump}
    :card-exchange #{:exchange-cards}
    :trick-playing (set (filter #(legal-play? game player %)
                               (get-in game [:players player :hand])))))

;; ============================================================================
;; Export internals for testing
;; ============================================================================

(def internal-states
  {:deck karbosh-deck
   :deal-hands deal-hands
   :init-game init-game
   :legal-play? legal-play?
   :resolve-trick resolve-trick
   :valid-bid? valid-bid?})