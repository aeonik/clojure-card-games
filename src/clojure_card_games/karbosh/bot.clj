(ns clojure-card-games.karbosh.bot
  (:require [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.probability.hypergeom :as hypergeom]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(defn suit-strength [hand suit]
  (reduce + (map #(rules/card-value % suit suit) hand)))

(defn best-trump [hand]
  (apply max-key #(suit-strength hand %) cards/suits))

(def default-bid-config
  {:bid-4-strength 3000
   :bid-5-strength 4300
   :bid-6-strength 6500
   :high-trump-strength 600
   :karbosh-min-trumps 6
   :karbosh-min-high-trumps 6
   :karbosh-min-bowers 2
   :karbosh-min-winners 7
   :karbosh-target-prob 0.72
   :karbosh-desperate-target-prob 0.66
   :karbosh-protect-target-prob 0.80
   :karbosh-score-context-band 12
   :karbosh-failure-opponent-tricks 1.2
   :karbosh-prob-intercept -3.9
   :karbosh-prob-trump-weight 0.22
   :karbosh-prob-high-trump-weight 0.38
   :karbosh-prob-bower-weight 0.55
   :karbosh-prob-right-bower-weight 0.20
   :karbosh-prob-left-bower-weight 0.16
   :karbosh-prob-off-ace-weight 0.18
   :karbosh-prob-low-trump-weight -0.20
   :karbosh-prob-off-junk-weight -0.35
   :karbosh-prob-missing-bower-weight -0.18
   :karbosh-prob-donation-help-weight 0.70
   :karbosh-prob-donation-pair-weight 0.40
   :karbosh-donation-qualification-prob 0.50
   :karbosh-donor-hand-size 8
   :double-karbosh-score-context-band 12})

(def ^:dynamic *bid-config* default-bid-config)

(def default-bid-strategy :karbosh-probability)

(def ^:dynamic *bid-strategy* default-bid-strategy)

(def classic-play-config
  {:lead-risk-tolerance 0.32
   :win-risk-tolerance 0.22
   :lead-risk-penalty 900
   :win-risk-penalty 700
   :karbosh-lead-risk-tolerance 0.03
   :karbosh-win-risk-tolerance 0.01
   :karbosh-lead-risk-penalty 6500
   :karbosh-win-risk-penalty 6500})

(def default-play-config
  (assoc classic-play-config :lead-risk-tolerance 0.05))

(def default-play-strategy :hybrid)

(def ^:dynamic *play-config* default-play-config)
(def ^:dynamic *play-strategy* default-play-strategy)

(defn trump-card? [trump card]
  (= trump (rules/effective-suit card trump)))

(defn high-trump? [config trump card]
  (>= (rules/card-value card trump trump)
      (:high-trump-strength config)))

(defn off-ace? [trump [rank :as card]]
  (and (= :A rank)
       (not (trump-card? trump card))))

(defn bower? [trump card]
  (or (rules/right-bower? card trump)
      (rules/left-bower? card trump)))

(defn trump-card-value [trump card]
  (rules/card-value card trump (rules/effective-suit card trump)))

(defn weakest-cards [trump n hand]
  (->> hand
       (sort-by #(trump-card-value trump %))
       (take n)
       vec))

(defn strongest-card-for-trump [trump hand]
  (first (sort-by #(trump-card-value trump %) > hand)))

(defn remove-first-card [hand card]
  (game/remove-first card hand))

(defn remove-cards [hand cards]
  (reduce remove-first-card (vec hand) cards))

(defn karbosh-discard-cards [hand trump]
  (weakest-cards trump 2 hand))

(defn helpful-donation-card? [config trump card]
  (or (bower? trump card)
      (high-trump? config trump card)
      (off-ace? trump card)))

(defn donation-hand-sizes [config game player]
  (let [default-size (:karbosh-donor-hand-size config)]
    (mapv (fn [partner]
            (let [n (count (get-in game [:players partner :hand]))]
              (if (pos? n) n default-size)))
          (game/partner-players game player))))

(defn donation-successful-hand-count-distribution
  [successes failures hand-sizes]
  (cond
    (or (neg? successes)
        (neg? failures)
        (some neg? hand-sizes))
    {}

    (empty? hand-sizes)
    {0 1}

    (< (+ successes failures) (reduce + hand-sizes))
    {}

    :else
    (let [hand-size (first hand-sizes)]
      (apply merge-with +
             (for [k (range 0 (inc (min hand-size successes)))
                   :let [failure-count (- hand-size k)]
                   :when (<= 0 failure-count failures)
                   :let [p (hypergeom/prob-hg successes failures hand-size k)
                         child (donation-successful-hand-count-distribution
                                 (- successes k)
                                 (- failures failure-count)
                                 (rest hand-sizes))
                         successful? (pos? k)]]
               (into {}
                     (map (fn [[n child-p]]
                            [(+ n (if successful? 1 0)) (* p child-p)]))
                     child))))))

(defn donation-probability-at-least-successful-hands
  [successes failures hand-sizes min-hands]
  (reduce +
          (for [[successful-hands p]
                (donation-successful-hand-count-distribution successes
                                                             failures
                                                             hand-sizes)
                :when (>= successful-hands min-hands)]
            p)))

(defn karbosh-donation-analysis [config game player trump]
  (let [hand (vec (get-in game [:players player :hand]))
        discards (karbosh-discard-cards hand trump)
        kept-hand (remove-cards hand discards)
        hidden (vec (analysis/remove-seen (cards/deck) hand))
        wanted (vec (filter #(helpful-donation-card? config trump %) hidden))
        successes (count wanted)
        failures (- (count hidden) successes)
        hand-sizes (donation-hand-sizes config game player)
        distribution (donation-successful-hand-count-distribution successes
                                                                  failures
                                                                  hand-sizes)
        expected-helpful (double (reduce + (map (fn [[n p]] (* n p))
                                                distribution)))
        prob-any (double (donation-probability-at-least-successful-hands
                          successes
                          failures
                          hand-sizes
                          1))
        prob-all (if (seq hand-sizes)
                   (double (donation-probability-at-least-successful-hands
                            successes
                            failures
                            hand-sizes
                            (count hand-sizes)))
                   0.0)]
    {:discards discards
     :kept-hand kept-hand
     :hidden-count (count hidden)
     :wanted-count successes
     :wanted-counts (frequencies wanted)
     :donor-hand-sizes hand-sizes
     :successful-donor-distribution distribution
     :expected-helpful-donations expected-helpful
     :prob-any-helpful-donation prob-any
     :prob-all-donors-helpful prob-all}))

(defn karbosh-hand? [config hand trump]
  (let [trumps (filter #(trump-card? trump %) hand)
        high-trumps (filter #(high-trump? config trump %) trumps)
        bowers (filter #(bower? trump %) trumps)
        winners (+ (count high-trumps)
                   (count (filter #(off-ace? trump %) hand)))]
    (and (some #(rules/right-bower? % trump) hand)
         (>= (count trumps) (:karbosh-min-trumps config))
         (>= (count high-trumps) (:karbosh-min-high-trumps config))
         (>= (count bowers) (:karbosh-min-bowers config))
         (>= winners (:karbosh-min-winners config)))))

(def top-trump-ranks
  [1 1 2 2 3 3 4 4 5 5 6 6 7 7])

(defn trump-control-rank [trump card]
  (let [[rank _] card]
    (cond
      (rules/right-bower? card trump) 1
      (rules/left-bower? card trump) 2
      (and (trump-card? trump card) (= :A rank)) 3
      (and (trump-card? trump card) (= :K rank)) 4
      (and (trump-card? trump card) (= :Q rank)) 5
      (and (trump-card? trump card) (= 10 rank)) 6
      (and (trump-card? trump card) (= 9 rank)) 7
      :else 99)))

(defn top-trump-prefix-count [hand trump]
  (loop [needed top-trump-ranks
         actual (sort (map #(trump-control-rank trump %)
                           (filter #(trump-card? trump %) hand)))
         prefix 0]
    (if (and (seq needed)
             (seq actual)
             (<= (first actual) (first needed)))
      (recur (rest needed) (rest actual) (inc prefix))
      prefix)))

(defn double-karbosh-features [hand trump]
  (let [hand (vec hand)
        trumps (filter #(trump-card? trump %) hand)
        prefix (top-trump-prefix-count hand trump)
        off-aces (filter #(off-ace? trump %) hand)
        straight? (>= prefix 8)
        forced? (or straight?
                    (and (= 8 (count trumps))
                         (>= prefix 6))
                    (and (= 7 (count trumps))
                         (>= prefix 7)
                         (seq off-aces)))]
    {:trump trump
     :trumps (count trumps)
     :top-trump-prefix prefix
     :off-aces (count off-aces)
     :straight? straight?
     :forced? (boolean forced?)}))

(defn numeric-target-bid [config hand]
  (let [trump (best-trump hand)
        strength (suit-strength hand trump)]
    (cond
      (>= strength (:bid-6-strength config)) {:type :bid :bid-type :bid :value 6}
      (>= strength (:bid-5-strength config)) {:type :bid :bid-type :bid :value 5}
      (>= strength (:bid-4-strength config)) {:type :bid :bid-type :bid :value 4}
      :else {:type :bid :bid-type :pass})))

(defn target-bid
  ([hand] (target-bid *bid-config* hand))
  ([config hand]
   (let [hand (vec hand)
         trump (best-trump hand)]
     (if (karbosh-hand? config hand trump)
       {:type :bid :bid-type :karbosh}
       (numeric-target-bid config hand)))))

(defn sigmoid [x]
  (/ 1.0 (+ 1.0 (Math/exp (- x)))))

(defn karbosh-features [config hand trump]
  (let [trumps (filter #(trump-card? trump %) hand)
        high-trumps (filter #(high-trump? config trump %) trumps)
        right-bowers (filter #(rules/right-bower? % trump) trumps)
        left-bowers (filter #(rules/left-bower? % trump) trumps)
        bowers (concat right-bowers left-bowers)
        off-cards (remove #(trump-card? trump %) hand)
        off-aces (filter #(off-ace? trump %) off-cards)
        low-trumps (remove #(high-trump? config trump %) trumps)
        off-junk (remove #(off-ace? trump %) off-cards)]
    {:trump trump
     :trumps (count trumps)
     :high-trumps (count high-trumps)
     :right-bowers (count right-bowers)
     :left-bowers (count left-bowers)
     :bowers (count bowers)
     :missing-bowers (- 4 (count bowers))
     :off-aces (count off-aces)
     :low-trumps (count low-trumps)
     :off-junk (count off-junk)
     :threshold-qualified? (karbosh-hand? config hand trump)}))

(defn donation-setup-qualified? [config features donation]
  (and (some? (:trump features))
       (pos? (:right-bowers features))
       (>= (:trumps features) (dec (:karbosh-min-trumps config)))
       (>= (:high-trumps features) (dec (:karbosh-min-high-trumps config)))
       (>= (:bowers features) (:karbosh-min-bowers config))
       (>= (:prob-any-helpful-donation donation)
           (:karbosh-donation-qualification-prob config))))

(defn karbosh-setup-analysis [config game player trump]
  (let [hand (vec (get-in game [:players player :hand]))
        features (karbosh-features config hand trump)
        donation (karbosh-donation-analysis config game player trump)]
    {:features features
     :donation donation
     :qualified? (or (:threshold-qualified? features)
                     (donation-setup-qualified? config features donation))}))

(defn weighted-karbosh-score [config features]
  (+ (:karbosh-prob-intercept config)
     (* (:karbosh-prob-trump-weight config) (:trumps features))
     (* (:karbosh-prob-high-trump-weight config) (:high-trumps features))
     (* (:karbosh-prob-bower-weight config) (:bowers features))
     (* (:karbosh-prob-right-bower-weight config) (:right-bowers features))
     (* (:karbosh-prob-left-bower-weight config) (:left-bowers features))
     (* (:karbosh-prob-off-ace-weight config) (:off-aces features))
     (* (:karbosh-prob-low-trump-weight config) (:low-trumps features))
     (* (:karbosh-prob-off-junk-weight config) (:off-junk features))
     (* (:karbosh-prob-missing-bower-weight config) (:missing-bowers features))))

(defn donation-karbosh-score [config donation]
  (+ (* (:karbosh-prob-donation-help-weight config)
        (:expected-helpful-donations donation))
     (* (:karbosh-prob-donation-pair-weight config)
        (:prob-all-donors-helpful donation))))

(defn team-score [game player]
  (get (:scores game) (game/player-team game player) 0))

(defn opponent-score [game player]
  (let [team (game/player-team game player)
        opponent (if (= team 1) 2 1)]
    (get (:scores game) opponent 0)))

(defn karbosh-target-prob [config game player]
  (let [own (team-score game player)
        opp (opponent-score game player)
        band (:karbosh-score-context-band config)]
    (cond
      (or (>= own (- game/target-score band))
          (>= (- own opp) band))
      (:karbosh-protect-target-prob config)

      (or (>= opp (- game/target-score band))
          (>= (- opp own) band))
      (:karbosh-desperate-target-prob config)

      :else
      (:karbosh-target-prob config))))

(defn desperate-score-context? [config game player]
  (let [own (team-score game player)
        opp (opponent-score game player)
        band (:double-karbosh-score-context-band config)]
    (or (>= opp (- game/target-score band))
        (>= (- opp own) band))))

(defn enemy-current-bid? [game player]
  (let [bid (game/current-bid game)]
    (and (:player bid)
         (not= (game/player-team game player)
               (game/player-team game (:player bid))))))

(defn enemy-karbosh-bid? [game player]
  (let [bid (game/current-bid game)]
    (and (= :karbosh (:bid-type bid))
         (enemy-current-bid? game player))))

(defn double-karbosh-evaluation [config game player trump]
  (let [features (double-karbosh-features
                  (get-in game [:players player :hand])
                  trump)
        straight? (:straight? features)
        desperate-overcall? (and (:forced? features)
                                 (enemy-karbosh-bid? game player)
                                 (desperate-score-context? config game player))]
    {:trump trump
     :features features
     :straight? straight?
     :desperate-overcall? (boolean desperate-overcall?)
     :call? (or straight? desperate-overcall?)}))

(defn karbosh-ev [config make-prob]
  (let [fail-prob (- 1.0 make-prob)
        fail-diff-loss (+ rules/special-bid-points
                          (:karbosh-failure-opponent-tricks config))]
    {:score-ev (+ (* make-prob rules/special-bid-points)
                  (* fail-prob (- rules/special-bid-points)))
     :diff-ev (+ (* make-prob rules/special-bid-points)
                 (* fail-prob (- fail-diff-loss)))
     :diff-break-even (/ fail-diff-loss
                         (+ rules/special-bid-points fail-diff-loss))}))

(defn karbosh-evaluation [config game player trump]
  (let [{:keys [features donation qualified?] :as setup}
        (karbosh-setup-analysis config game player trump)
        make-prob (sigmoid (+ (weighted-karbosh-score config features)
                              (donation-karbosh-score config donation)))
        target-prob (karbosh-target-prob config game player)
        ev (karbosh-ev config make-prob)]
    (assoc ev
           :trump trump
           :features features
           :donation donation
           :setup setup
           :make-prob make-prob
           :target-prob target-prob
           :call? (and qualified?
                       (>= make-prob target-prob)
                       (pos? (:diff-ev ev))))))

(defn threshold-bid-action [game player]
  (let [candidate (target-bid (get-in game [:players player :hand]))
        current-rank (rules/bid-rank (game/current-bid game))]
    (if (> (rules/bid-rank candidate) current-rank)
      candidate
      {:type :bid :bid-type :pass})))

(defn probability-bid-action [game player]
  (let [hand (get-in game [:players player :hand])
        trump (best-trump hand)
        double-karbosh (double-karbosh-evaluation *bid-config*
                                                  game
                                                  player
                                                  trump)
        karbosh (karbosh-evaluation *bid-config* game player trump)
        candidate (cond
                    (:call? double-karbosh)
                    {:type :bid :bid-type :double-karbosh}

                    (:call? karbosh)
                    {:type :bid :bid-type :karbosh}

                    :else
                    (numeric-target-bid *bid-config* hand))
        current-rank (rules/bid-rank (game/current-bid game))]
    (if (> (rules/bid-rank candidate) current-rank)
      candidate
      {:type :bid :bid-type :pass})))

(def bid-strategies
  {:karbosh-threshold threshold-bid-action
   :karbosh-probability probability-bid-action})

(defn resolve-bid-strategy [strategy]
  (cond
    (fn? strategy) strategy
    (keyword? strategy) (or (get bid-strategies strategy)
                            (throw (ex-info "Unknown bid strategy"
                                            {:strategy strategy
                                             :available (keys bid-strategies)})))
    :else (throw (ex-info "Invalid bid strategy" {:strategy strategy}))))

(defn bid-action
  ([game player]
   (bid-action game player *bid-strategy*))
  ([game player strategy]
   ((resolve-bid-strategy strategy) game player)))

(defn trump-action [game player]
  {:type :trump-selection
   :suit (best-trump (get-in game [:players player :hand]))})

(declare highest-card lowest-card)

(defn donate-action [game player]
  (when-let [card (highest-card game (get-in game [:players player :hand]))]
    {:type :donate-card
     :card card}))

(defn discard-action [game player]
  (when-let [card (lowest-card game (get-in game [:players player :hand]))]
    {:type :discard-card
     :card card}))

(defn legal-cards [game player]
  (let [hand (get-in game [:players player :hand])]
    (rules/legal-cards hand (:current-trick game) (:trump game))))

(defn completed-trick-cards [game]
  (analysis/completed-trick-cards game))

(defn current-trick-cards [game]
  (analysis/current-trick-cards game))

(defn public-played-cards [game]
  (analysis/public-played-cards game))

(defn seen-cards [game player]
  (analysis/seen-cards game player))

(defn unseen-cards [game player]
  (analysis/unseen-cards game player))

(defn unseen-card-counts [game player]
  (analysis/unseen-card-counts game player))

(defn hypergeom-analysis [game player]
  (analysis/player-analysis game player))

(defn card-score [game card]
  (let [lead (or (some-> (:current-trick game) first :card
                         (rules/effective-suit (:trump game)))
                 (rules/effective-suit card (:trump game)))]
    (rules/card-value card (:trump game) lead)))

(defn current-trick-winner [game]
  (when (seq (:current-trick game))
    (rules/resolve-trick (:current-trick game) (:trump game))))

(defn same-team? [game a b]
  (and a b (= (get-in game [:players a :team])
              (get-in game [:players b :team]))))

(def special-bid-types #{:karbosh :double-karbosh})

(defn special-contract? [bid]
  (contains? special-bid-types (:bid-type bid)))

(defn special-contract-caller? [game player]
  (let [bid (game/current-bid game)]
    (and (special-contract? bid)
         (= player (:player bid)))))

(defn contract-caller? [game player]
  (= player (:player (game/current-bid game))))

(defn context-play-config [config game player]
  (if (special-contract-caller? game player)
    (assoc config
           :lead-risk-tolerance (:karbosh-lead-risk-tolerance config)
           :win-risk-tolerance (:karbosh-win-risk-tolerance config)
           :lead-risk-penalty (:karbosh-lead-risk-penalty config)
           :win-risk-penalty (:karbosh-win-risk-penalty config))
    config))

(defn wins-trick? [game player card]
  (= player (rules/resolve-trick (conj (:current-trick game)
                                       {:player player :card card})
                                 (:trump game))))

(defn can-be-beaten-by? [game unseen-counts card lead]
  (let [trump (:trump game)
        value (rules/card-value card trump lead)]
    (some (fn [[hidden-card n]]
            (and (pos? n)
                 (> (rules/card-value hidden-card trump lead) value)))
          unseen-counts)))

(defn can-be-beaten-in-suit-by? [game unseen-counts card lead]
  (let [trump (:trump game)
        value (rules/card-value card trump lead)]
    (some (fn [[hidden-card n]]
            (and (pos? n)
                 (= lead (rules/effective-suit hidden-card trump))
                 (> (rules/card-value hidden-card trump lead) value)))
          unseen-counts)))

(defn can-be-beaten? [game player card lead]
  (can-be-beaten-by? game (unseen-card-counts game player) card lead))

(defn good-card-with-counts? [game unseen-counts card]
  (let [lead (or (rules/trick-lead (:current-trick game) (:trump game))
                 (rules/effective-suit card (:trump game)))]
    (not (can-be-beaten-by? game unseen-counts card lead))))

(defn good-card? [game player card]
  (good-card-with-counts? game (unseen-card-counts game player) card))

(defn secure-winning-card-with-counts? [game player unseen-counts card]
  (and (wins-trick? game player card)
       (good-card-with-counts? game unseen-counts card)))

(defn secure-winning-card? [game player card]
  (secure-winning-card-with-counts? game
                                    player
                                    (unseen-card-counts game player)
                                    card))

(defn secure-trump-lead-card [game unseen-counts cards]
  (let [trumps (filter #(trump-card? (:trump game) %) cards)
        secure-trumps (filter #(good-card-with-counts? game unseen-counts %)
                              trumps)]
    (when (seq secure-trumps)
      (highest-card game secure-trumps))))

(defn low-trump? [trump card]
  (and (trump-card? trump card)
       (< (rules/card-value card trump trump)
          (rules/card-value [:Q trump] trump trump))))

(defn caller-pressure-lead-card [game cards]
  (or (when-let [off-aces (seq (filter #(off-ace? (:trump game) %) cards))]
        (lowest-card game off-aces))
      (when-let [low-trumps (seq (filter #(low-trump? (:trump game) %) cards))]
        (lowest-card game low-trumps))))

(defn remaining-trick-players-after [game player]
  (let [remaining (- (count (game/trick-players game))
                     (count (:current-trick game))
                     1)]
    (take (max 0 remaining)
          (rest (iterate #(game/next-trick-player game %) player)))))

(defn pending-opponents-after [game player]
  (remove #(same-team? game player %)
          (remaining-trick-players-after game player)))

(defn suit-protecting-card? [game player unseen-counts lead card]
  (and (= lead (rules/effective-suit card (:trump game)))
       (wins-trick? game player card)
       (not (can-be-beaten-in-suit-by? game unseen-counts card lead))))

(defn partner-protecting-card [game player unseen-counts cards]
  (let [lead (rules/trick-lead (:current-trick game) (:trump game))
        winner-card (:card (rules/winning-play (:current-trick game)
                                               (:trump game)))
        vulnerable? (and lead
                         (seq (pending-opponents-after game player))
                         (can-be-beaten-in-suit-by? game
                                                    unseen-counts
                                                    winner-card
                                                    lead))
        protectors (filter #(suit-protecting-card?
                              game player unseen-counts lead %)
                           cards)]
    (when (and vulnerable? (seq protectors))
      (lowest-card game protectors))))

(defn partner-preserving-card [game player unseen-counts cards]
  (let [non-overtakers (remove #(wins-trick? game player %) cards)]
    (or (partner-protecting-card game player unseen-counts cards)
        (lowest-card game (or (seq non-overtakers) cards)))))

(defn lowest-card [game cards]
  (first (sort-by #(card-score game %) cards)))

(defn highest-card [game cards]
  (first (sort-by #(card-score game %) > cards)))

(defn card-counting-card-action [game player]
  (let [cards (vec (legal-cards game player))
        winner (current-trick-winner game)
        unseen-counts (unseen-card-counts game player)
        winning-cards (filter #(wins-trick? game player %) cards)
        good-cards (filter #(good-card-with-counts? game unseen-counts %) cards)
        secure-winning-cards (filter #(secure-winning-card-with-counts?
                                        game
                                        player
                                        unseen-counts
                                        %)
                                     winning-cards)
        card (cond
               (empty? cards)
               nil

               (empty? (:current-trick game))
               (if (seq good-cards)
                 (lowest-card game good-cards)
                 (highest-card game cards))

               (same-team? game player winner)
               (partner-preserving-card game player unseen-counts cards)

               (seq secure-winning-cards)
               (lowest-card game secure-winning-cards)

               (seq winning-cards)
               (lowest-card game winning-cards)

               :else
               (lowest-card game cards))]
    (when card
      {:type :play-card
       :card card})))

(defn card-analyses [game player cards]
  (let [trump (:trump game)
        unseen (vec (unseen-cards game player))
        population-size (count unseen)
        counts (analysis/effective-suit-counts trump unseen)]
    (into {}
          (map (fn [card]
                 [card
                  (analysis/card-defeat-analysis game
                                                 player
                                                 trump
                                                 unseen
                                                 counts
                                                 population-size
                                                 card)]))
          cards)))

(defn probability [x]
  (double (or x 0)))

(defn card-risk [analyses card]
  (probability (get-in analyses [card :prob-pending-opponent-can-beat-card])))

(defn risk-adjusted-lead-value [config game analyses card]
  (- (card-score game card)
     (* (:lead-risk-penalty config) (card-risk analyses card))))

(defn risk-adjusted-win-cost [config game analyses card]
  (+ (card-score game card)
     (* (:win-risk-penalty config) (card-risk analyses card))))

(defn safe-cards [config analyses threshold-key cards]
  (filter #(<= (card-risk analyses %) (threshold-key config)) cards))

(defn priority-lead-card [game player cards]
  (let [unseen-counts (unseen-card-counts game player)
        trump-control (when (contract-caller? game player)
                        (secure-trump-lead-card game unseen-counts cards))
        caller-pressure (when (contract-caller? game player)
                          (caller-pressure-lead-card game cards))]
    (or trump-control caller-pressure)))

(defn threshold-lead-card [config game player analyses cards]
  (let [priority (priority-lead-card game player cards)
        safe (safe-cards config analyses :lead-risk-tolerance cards)]
    (cond
      priority
      priority

      (seq safe)
      (lowest-card game safe)

      :else
      (lowest-card game cards))))

(defn risk-adjusted-lead-candidates [game player cards]
  (let [trump (:trump game)
        off-aces (seq (filter #(off-ace? trump %) cards))]
    (cond
      (contract-caller? game player)
      (or (seq (remove #(trump-card? trump %) cards))
          cards)

      off-aces
      off-aces

      :else
      cards)))

(defn defender-exit-lead-candidates [game player cards]
  (let [trump (:trump game)
        off-aces (seq (filter #(off-ace? trump %) cards))
        non-trumps (seq (remove #(trump-card? trump %) cards))]
    (cond
      (contract-caller? game player)
      (or non-trumps cards)

      off-aces
      off-aces

      non-trumps
      non-trumps

      :else
      cards)))

(defn probability-lead-card-with-candidates
  [candidate-fn config game player analyses cards]
  (let [priority (priority-lead-card game player cards)
        safe (safe-cards config analyses :lead-risk-tolerance cards)
        fallback-cards (candidate-fn game player cards)]
    (cond
      priority
      priority

      (seq safe)
      (lowest-card game safe)

      :else
      (first (sort-by #(risk-adjusted-lead-value config game analyses %)
                      >
                      fallback-cards)))))

(defn probability-lead-card [config game player analyses cards]
  (probability-lead-card-with-candidates risk-adjusted-lead-candidates
                                         config
                                         game
                                         player
                                         analyses
                                         cards))

(defn defender-exit-probability-lead-card [config game player analyses cards]
  (probability-lead-card-with-candidates defender-exit-lead-candidates
                                         config
                                         game
                                         player
                                         analyses
                                         cards))

(defn karbosh-caller-lead-card [config game player analyses cards]
  (let [trumps (filter #(trump-card? (:trump game) %) cards)]
    (if (seq trumps)
      (first (sort-by #(risk-adjusted-lead-value config game analyses %)
                      >
                      trumps))
      (probability-lead-card config game player analyses cards))))

(defn probability-winning-card [config game analyses cards]
  (let [safe (safe-cards config analyses :win-risk-tolerance cards)]
    (if (seq safe)
      (lowest-card game safe)
      (first (sort-by #(risk-adjusted-win-cost config game analyses %)
                      cards)))))

(defn probability-card-action-with-lead [lead-card-fn game player]
  (let [cards (vec (legal-cards game player))
        winner (current-trick-winner game)
        config (context-play-config *play-config* game player)
        unseen-counts (unseen-card-counts game player)
        analyses (card-analyses game player cards)
        winning-cards (filter #(wins-trick? game player %) cards)
        card (cond
               (empty? cards)
               nil

               (empty? (:current-trick game))
               (if (special-contract-caller? game player)
                 (karbosh-caller-lead-card config game player analyses cards)
                 (lead-card-fn config game player analyses cards))

               (same-team? game player winner)
               (partner-preserving-card game player unseen-counts cards)

               (seq winning-cards)
               (probability-winning-card config game analyses winning-cards)

               :else
               (lowest-card game cards))]
    (when card
      {:type :play-card
       :card card})))

(defn threshold-probability-card-action [game player]
  (probability-card-action-with-lead threshold-lead-card game player))

(defn probability-card-action [game player]
  (probability-card-action-with-lead probability-lead-card game player))

(defn defender-exit-probability-card-action [game player]
  (probability-card-action-with-lead defender-exit-probability-lead-card
                                     game
                                     player))

(defn hybrid-threshold-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (threshold-probability-card-action game player)))

(defn hybrid-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (probability-card-action game player)))

(defn defender-exit-hybrid-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (defender-exit-probability-card-action game player)))

(def play-strategies
  {:card-counting card-counting-card-action
   :probability-threshold threshold-probability-card-action
   :probability probability-card-action
   :probability-defender-exit defender-exit-probability-card-action
   :hybrid-threshold hybrid-threshold-card-action
   :hybrid hybrid-card-action
   :hybrid-defender-exit defender-exit-hybrid-card-action})

(defn resolve-play-strategy [strategy]
  (cond
    (fn? strategy) strategy
    (keyword? strategy) (or (get play-strategies strategy)
                            (throw (ex-info "Unknown play strategy"
                                            {:strategy strategy
                                             :available (keys play-strategies)})))
    :else (throw (ex-info "Invalid play strategy" {:strategy strategy}))))

(defn card-action
  ([game player]
   (card-action game player *play-strategy*))
  ([game player strategy]
   ((resolve-play-strategy strategy) game player)))

(defn action [game player]
  (case (:phase game)
    :bidding (bid-action game player)
    :trump-selection (trump-action game player)
    :karbosh-donation (donate-action game player)
    :karbosh-discard (discard-action game player)
    :trick-playing (card-action game player)
    nil))
