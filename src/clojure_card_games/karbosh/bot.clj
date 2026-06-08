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
   :double-karbosh-score-context-band 12
   :conservative-bid-min-controls {4 3
                                    5 4
                                    6 6}})

(def ^:dynamic *bid-config* default-bid-config)

(def default-bid-strategy :karbosh-probability)

(def ^:dynamic *bid-strategy* default-bid-strategy)

(def default-ditch-policy :future-suit-equity)
(def classic-ditch-policy :classic)
(def ditch-policies #{default-ditch-policy classic-ditch-policy})

(def classic-play-config
  {:lead-risk-tolerance 0.32
   :win-risk-tolerance 0.22
   :lead-risk-penalty 900
   :win-risk-penalty 700
   :defender-high-trump-preservation-penalty 2000
   :team-ev-trick-weight 1000
   :team-ev-partner-ruff-weight 1300
   :team-ev-opponent-ruff-penalty 900
   :team-ev-risk-penalty 700
   :team-ev-high-trump-spend-penalty 650
   :team-ev-card-spend-rate 0.12
   :team-ev-safe-card-bonus 150
   :ditch-policy default-ditch-policy
   :ditch-future-suit-equity-weight 50
   :soft-void-trump-threshold 0.65
   :karbosh-lead-risk-tolerance 0.03
   :karbosh-win-risk-tolerance 0.01
   :karbosh-lead-risk-penalty 6500
   :karbosh-win-risk-penalty 6500})

(def default-play-config
  (assoc classic-play-config :lead-risk-tolerance 0.05))

(def default-play-strategy :hybrid-preservation)

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

(defn numeric-bid-features [config hand trump]
  (let [hand (vec hand)
        trumps (filter #(trump-card? trump %) hand)
        high-trumps (filter #(high-trump? config trump %) trumps)
        bowers (filter #(bower? trump %) trumps)
        off-aces (filter #(off-ace? trump %) hand)
        bower-bonus (max 0 (dec (count bowers)))]
    {:trump trump
     :strength (suit-strength hand trump)
     :trumps (count trumps)
     :high-trumps (count high-trumps)
     :bowers (count bowers)
     :off-aces (count off-aces)
     :controls (+ (count high-trumps)
                  (count off-aces)
                  bower-bonus)}))

(defn conservative-numeric-bid? [config hand bid]
  (let [trump (best-trump hand)
        features (numeric-bid-features config hand trump)
        min-controls (get (:conservative-bid-min-controls config)
                          (:value bid)
                          0)]
    (>= (:controls features) min-controls)))

(defn conservative-numeric-target-bid [config hand]
  (let [candidate (numeric-target-bid config hand)]
    (if (and (= :bid (:bid-type candidate))
             (not (conservative-numeric-bid? config hand candidate)))
      {:type :bid :bid-type :pass}
      candidate)))

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

(defn playable-bid [game candidate]
  (let [current-rank (rules/bid-rank (game/current-bid game))]
    (if (> (rules/bid-rank candidate) current-rank)
      candidate
      {:type :bid :bid-type :pass})))

(defn probability-bid-candidate [numeric-target-fn game player]
  (let [hand (get-in game [:players player :hand])
        trump (best-trump hand)
        double-karbosh (double-karbosh-evaluation *bid-config*
                                                  game
                                                  player
                                                  trump)
        karbosh (karbosh-evaluation *bid-config* game player trump)]
    (cond
      (:call? double-karbosh)
      {:type :bid :bid-type :double-karbosh}

      (:call? karbosh)
      {:type :bid :bid-type :karbosh}

      :else
      (numeric-target-fn *bid-config* hand))))

(defn probability-bid-action [game player]
  (playable-bid game
                (probability-bid-candidate numeric-target-bid game player)))

(defn conservative-probability-bid-action [game player]
  (playable-bid game
                (probability-bid-candidate conservative-numeric-target-bid
                                           game
                                           player)))

(def bid-strategies
  {:karbosh-threshold threshold-bid-action
   :karbosh-probability probability-bid-action
   :karbosh-probability-conservative conservative-probability-bid-action})

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

(declare ditch-card highest-card lowest-card)

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

(defn maker-team? [game player]
  (when-let [bid (game/current-bid game)]
    (= (game/player-team game player)
       (game/player-team game (:player bid)))))

(defn lead-context [game player]
  (let [bid (game/current-bid game)
        maker? (boolean (and bid (maker-team? game player)))]
    {:bid bid
     :trump (:trump game)
     :caller? (= player (:player bid))
     :maker-team? maker?
     :defender? (boolean (and bid (not maker?)))}))

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

(defn pending-partners-after [game player]
  (filter #(same-team? game player %)
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

(defn partner-preserving-card
  ([game player unseen-counts cards]
   (partner-preserving-card default-play-config game player unseen-counts cards))
  ([config game player unseen-counts cards]
   (let [non-overtakers (remove #(wins-trick? game player %) cards)]
     (or (partner-protecting-card game player unseen-counts cards)
         (ditch-card config
                     game
                     player
                     unseen-counts
                     (or (seq non-overtakers) cards))))))

(defn lowest-card [game cards]
  (first (sort-by #(card-score game %) cards)))

(defn highest-card [game cards]
  (first (sort-by #(card-score game %) > cards)))

(defn card-effective-suit [game card]
  (rules/effective-suit card (:trump game)))

(defn potential-card-score [game card]
  (rules/card-value card (:trump game) (card-effective-suit game card)))

(defn in-suit-control-card? [game unseen-counts card]
  (not (can-be-beaten-in-suit-by? game
                                  unseen-counts
                                  card
                                  (card-effective-suit game card))))

(defn remaining-hand-after [game player card]
  (remove-first-card (get-in game [:players player :hand]) card))

(defn suit-counts [game hand]
  (frequencies (map #(card-effective-suit game %) hand)))

(defn same-suit-control-after-discard? [game player unseen-counts card]
  (let [suit (card-effective-suit game card)
        score (potential-card-score game card)]
    (some #(and (= suit (card-effective-suit game %))
                (>= (potential-card-score game %) score)
                (in-suit-control-card? game unseen-counts %))
          (remaining-hand-after game player card))))

(defn higher-follow-count-with-counts [game unseen-counts card]
  (let [trump (:trump game)
        lead (card-effective-suit game card)]
    (reduce-kv (fn [n hidden-card cnt]
                 (if (and (pos? cnt)
                          (= lead (rules/effective-suit hidden-card trump))
                          (rules/beats? trump lead hidden-card card))
                   (+ n cnt)
                   n))
               0
               unseen-counts)))

(defn future-opponent-hand-sizes [game player]
  (vals (analysis/hand-sizes
          game
          (analysis/opponent-players game player (game/trick-players game)))))

(defn future-suit-control-probability [game player unseen-counts card]
  (let [population-size (reduce + (vals unseen-counts))
        higher-follow-count (higher-follow-count-with-counts game unseen-counts card)
        opponent-hand-sizes (filter pos? (future-opponent-hand-sizes game player))
        higher-follow-risk (analysis/probability-of-any-success
                             higher-follow-count
                             population-size
                             opponent-hand-sizes)]
    (if (seq opponent-hand-sizes)
      (- 1.0 higher-follow-risk)
      0.0)))

(defn future-suit-equity [game player unseen-counts card]
  (if (trump-card? (:trump game) card)
    0.0
    (* (potential-card-score game card)
       (future-suit-control-probability game player unseen-counts card))))

(defn best-same-suit-equity-after-discard [game player unseen-counts card]
  (let [suit (card-effective-suit game card)]
    (reduce max
            0.0
            (map #(if (= suit (card-effective-suit game %))
                    (future-suit-equity game player unseen-counts %)
                    0.0)
                 (remaining-hand-after game player card)))))

(defn future-suit-equity-loss [game player unseen-counts card]
  (max 0.0
       (- (future-suit-equity game player unseen-counts card)
          (best-same-suit-equity-after-discard
            game
            player
            unseen-counts
            card))))

(defn ditch-future-suit-equity-weight [config]
  (case (:ditch-policy config default-ditch-policy)
    :classic 0
    :future-suit-equity (:ditch-future-suit-equity-weight config 50)
    (:ditch-future-suit-equity-weight config 50)))

(defn ditch-card-cost [config game player unseen-counts cards card]
  (let [trump (:trump game)
        hand (get-in game [:players player :hand])
        counts (suit-counts game hand)
        suit (card-effective-suit game card)
        suit-count (get counts suit 0)
        remaining-suit-count (max 0 (dec suit-count))
        trump? (trump-card? trump card)
        has-trump? (some #(trump-card? trump %) hand)
        non-trump-legal? (some #(not (trump-card? trump %)) cards)
        control? (in-suit-control-card? game unseen-counts card)
        last-control? (and control?
                           (not (same-suit-control-after-discard?
                                  game
                                  player
                                  unseen-counts
                                  card)))
        future-equity-loss (future-suit-equity-loss
                             game
                             player
                             unseen-counts
                             card)
        short-suit-bonus (if (and has-trump? (not trump?))
                           (case remaining-suit-count
                             0 600
                             1 250
                             0)
                           0)]
    (- (+ (potential-card-score game card)
          (if (and trump? non-trump-legal?) 5000 0)
          (if last-control? 3000 0)
          (* (ditch-future-suit-equity-weight config)
             future-equity-loss)
          (* 8 suit-count))
       short-suit-bonus)))

(defn ditch-card
  "Choose a card to throw away when this play is not trying to win the trick.

  The ranking preserves trump, exact suit controls, and future suit equity
  estimated from unseen cards, while using low off-suit cards to clear suits
  when the player has trump left for future ruffs."
  ([game player unseen-counts cards]
   (ditch-card default-play-config game player unseen-counts cards))
  ([config game player unseen-counts cards]
   (first (sort-by #(ditch-card-cost config game player unseen-counts cards %)
                   cards))))

(defn trick-known-voids [trump trick]
  (let [lead (rules/trick-lead trick trump)]
    (when lead
      (keep (fn [{:keys [player card]}]
              (when (not= lead (rules/effective-suit card trump))
                [player lead]))
            trick))))

(defn known-voids [game]
  (let [trump (:trump game)]
    (reduce (fn [voids [player suit]]
              (update voids player (fnil conj #{}) suit))
            {}
            (mapcat #(trick-known-voids trump %)
                    (:completed-tricks game)))))

(defn known-void? [voids player suit]
  (contains? (get voids player #{}) suit))

(defn soft-void-discard-confidence
  "Estimate whether a discard suggests the player was also void in `target-suit`.

  This is deliberately softer than `known-voids`: it never changes legality or
  exact card-count facts. It only lets policy code reason that throwing away a
  valuable off-suit card often means the player lacked a useful trump ruff."
  [game target-suit card]
  (let [trump (:trump game)
        effective (rules/effective-suit card trump)
        score (potential-card-score game card)]
    (cond
      (or (nil? target-suit)
          (not= target-suit trump)
          (= target-suit effective))
      0.0

      (>= score 80)
      0.85

      (>= score 70)
      0.72

      (>= score 60)
      0.55

      (>= score 50)
      0.40

      :else
      0.15)))

(defn completed-and-current-tricks [game]
  (cond-> (vec (:completed-tricks game))
    (seq (:current-trick game)) (conj (:current-trick game))))

(defn soft-void-evidence [game target-player target-suit]
  (let [trump (:trump game)]
    (keep (fn [trick]
            (when-let [lead (rules/trick-lead trick trump)]
              (some (fn [{:keys [player card]}]
                      (when (and (= player target-player)
                                 (not= lead (rules/effective-suit card trump)))
                        (soft-void-discard-confidence game target-suit card)))
                    trick)))
          (completed-and-current-tricks game))))

(defn soft-void-confidence [game voids player suit]
  (if (known-void? voids player suit)
    1.0
    (analysis/combine-event-probabilities
     (soft-void-evidence game player suit))))

(defn likely-void? [config game voids player suit]
  (>= (soft-void-confidence game voids player suit)
      (:soft-void-trump-threshold config 0.65)))

(defn players-with-cards [game players]
  (filter #(pos? (count (get-in game [:players % :hand]))) players))

(defn unseen-effective-suit-count [unseen-counts trump suit]
  (reduce-kv (fn [n card count]
               (if (= suit (rules/effective-suit card trump))
                 (+ n count)
                 n))
             0
             unseen-counts))

(defn opponents-likely-void-in-suit? [config game voids player suit]
  (every? #(likely-void? config game voids % suit)
          (players-with-cards
           game
           (remove #(same-team? game player %) (game/trick-players game)))))

(defn partners-known-void-in-suit? [game voids player suit]
  (let [partners (players-with-cards game (game/partner-players game player))]
    (and (seq partners)
         (every? #(known-void? voids % suit) partners))))

(defn partner-ruff-invite-card
  ([game player unseen-counts cards]
   (partner-ruff-invite-card default-play-config game player unseen-counts cards))
  ([config game player unseen-counts cards]
   (let [trump (:trump game)
         voids (known-voids game)
         secure-trump (secure-trump-lead-card game unseen-counts cards)
         unseen-trumps (unseen-effective-suit-count unseen-counts trump trump)
         off-suit-cards (remove #(trump-card? trump %) cards)
         partner-void-cards (filter #(partners-known-void-in-suit?
                                       game
                                       voids
                                       player
                                       (rules/effective-suit % trump))
                                    off-suit-cards)]
     (when (and trump
                secure-trump
                (pos? unseen-trumps)
                (opponents-likely-void-in-suit? config game voids player trump)
                (seq partner-void-cards))
       (lowest-card game partner-void-cards)))))

(defn card-counting-card-action [game player]
  (let [cards (vec (legal-cards game player))
        winner (current-trick-winner game)
        config (context-play-config *play-config* game player)
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
               (partner-preserving-card config game player unseen-counts cards)

               (seq secure-winning-cards)
               (lowest-card game secure-winning-cards)

               (seq winning-cards)
               (lowest-card game winning-cards)

               :else
               (ditch-card config game player unseen-counts cards))]
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

(defn round-probability [x]
  (/ (Math/round (* 1000.0 (probability x))) 1000.0))

(defn card-risk [analyses card]
  (probability (get-in analyses [card :prob-pending-opponent-can-beat-card])))

(defn high-preservation-trump? [game card]
  (and (trump-card? (:trump game) card)
       (>= (card-score game card)
           (rules/card-value [:K (:trump game)] (:trump game) (:trump game)))))

(defn lead-card-features [game analyses card]
  (let [risk (card-risk analyses card)]
    {:card card
     :score (card-score game card)
     :risk risk
     :trump? (trump-card? (:trump game) card)
     :high-trump? (high-preservation-trump? game card)
     :good? (zero? risk)}))

(defn risk-adjusted-lead-value [config game analyses card]
  (- (card-score game card)
     (* (:lead-risk-penalty config) (card-risk analyses card))))

(defn defender-preservation-penalty [config context features]
  (if (and (:defender? context)
           (:trump? features)
           (:high-trump? features)
           (not (:good? features)))
    (* (or (:defender-high-trump-preservation-penalty config) 0)
       (:risk features))
    0.0))

(defn preservation-lead-value [config game context analyses card]
  (let [{:keys [score risk] :as features} (lead-card-features game analyses card)]
    (- score
       (* (:lead-risk-penalty config) risk)
       (defender-preservation-penalty config context features))))

(defn best-lead-by-value [value-fn cards]
  (first (sort-by value-fn > cards)))

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

(defn defender-low-exit-card [game player cards]
  (let [{:keys [defender?]} (lead-context game player)
        trump (:trump game)
        off-aces (seq (filter #(off-ace? trump %) cards))
        non-trumps (seq (remove #(trump-card? trump %) cards))]
    (when (and defender?
               non-trumps
               (not off-aces))
      (lowest-card game non-trumps))))

(declare lower-preservation-winners)

(defn preservation-trump-lead-card [config game analyses cards]
  (when (and (seq cards)
             (every? #(trump-card? (:trump game) %) cards))
    (when-let [lower-winners (seq (lower-preservation-winners config
                                                              game
                                                              analyses
                                                              cards))]
      (lowest-card game lower-winners))))

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
      (best-lead-by-value #(risk-adjusted-lead-value config
                                                    game
                                                    analyses
                                                    %)
                          fallback-cards))))

(defn probability-lead-card [config game player analyses cards]
  (probability-lead-card-with-candidates risk-adjusted-lead-candidates
                                         config
                                         game
                                         player
                                         analyses
                                         cards))

(defn defender-exit-probability-lead-card [config game player analyses cards]
  (let [priority (priority-lead-card game player cards)
        safe (safe-cards config analyses :lead-risk-tolerance cards)
        defender-low-exit (defender-low-exit-card game player cards)
        fallback-cards (defender-exit-lead-candidates game player cards)]
    (cond
      priority
      priority

      (seq safe)
      (lowest-card game safe)

      defender-low-exit
      defender-low-exit

      :else
      (best-lead-by-value #(risk-adjusted-lead-value config
                                                    game
                                                    analyses
                                                    %)
                          fallback-cards))))

(defn preservation-probability-lead-card [config game player analyses cards]
  (let [priority (priority-lead-card game player cards)
        safe (safe-cards config analyses :lead-risk-tolerance cards)
        defender-low-exit (defender-low-exit-card game player cards)
        trump-lead (preservation-trump-lead-card config game analyses cards)
        fallback-cards (risk-adjusted-lead-candidates game player cards)
        context (lead-context game player)]
    (cond
      priority
      priority

      (seq safe)
      (lowest-card game safe)

      defender-low-exit
      defender-low-exit

      trump-lead
      trump-lead

      :else
      (best-lead-by-value #(preservation-lead-value config
                                                    game
                                                    context
                                                    analyses
                                                    %)
                          fallback-cards))))

(defn ruff-invite-preservation-lead-card [config game player analyses cards]
  (or (partner-ruff-invite-card config
                                game
                                player
                                (unseen-card-counts game player)
                                cards)
      (preservation-probability-lead-card config game player analyses cards)))

(defn total-unseen-count [unseen-counts]
  (reduce + (vals unseen-counts)))

(defn prob-hand-has-success [successes population-size hand-size]
  (if (and (pos? successes)
           (pos? hand-size)
           (<= hand-size population-size))
    (probability (analysis/probability-of-any-success successes
                                                      population-size
                                                      [hand-size]))
    0.0))

(defn prob-void-and-trump-for-player
  ([game player unseen-counts voids lead other]
   (prob-void-and-trump-for-player default-play-config
                                   game
                                   player
                                   unseen-counts
                                   voids
                                   lead
                                   other))
  ([_config game _player unseen-counts voids lead other]
   (let [trump (:trump game)
         population-size (total-unseen-count unseen-counts)
         hand-size (count (get-in game [:players other :hand]))
         trump-left (unseen-effective-suit-count unseen-counts trump trump)
         lead-left (unseen-effective-suit-count unseen-counts trump lead)
         trump-available-prob (- 1.0
                                 (soft-void-confidence game
                                                       voids
                                                       other
                                                       trump))]
     (cond
       (or (nil? trump)
           (= lead trump)
           (not (pos? hand-size))
           (not (pos? trump-available-prob)))
       0.0

       (known-void? voids other lead)
       (* trump-available-prob
          (prob-hand-has-success trump-left population-size hand-size))

       :else
       (* trump-available-prob
          (probability
           (analysis/probability-specific-void-and-trump
            lead-left
            trump-left
            population-size
            hand-size)))))))

(defn combined-probability [probabilities]
  (analysis/combine-event-probabilities probabilities))

(defn partner-ruff-probability-for-lead
  ([game player unseen-counts lead]
   (partner-ruff-probability-for-lead default-play-config
                                      game
                                      player
                                      unseen-counts
                                      lead))
  ([config game player unseen-counts lead]
   (let [voids (known-voids game)]
     (combined-probability
      (map #(prob-void-and-trump-for-player config
                                            game
                                            player
                                            unseen-counts
                                            voids
                                            lead
                                            %)
           (players-with-cards game (pending-partners-after game player)))))))

(defn opponent-ruff-probability-for-lead
  ([game player unseen-counts lead]
   (opponent-ruff-probability-for-lead default-play-config
                                       game
                                       player
                                       unseen-counts
                                       lead))
  ([config game player unseen-counts lead]
   (let [voids (known-voids game)]
     (combined-probability
      (map #(prob-void-and-trump-for-player config
                                            game
                                            player
                                            unseen-counts
                                            voids
                                            lead
                                            %)
           (players-with-cards game (pending-opponents-after game player)))))))

(defn card-spend-cost [config game card]
  (+ (* (:team-ev-card-spend-rate config) (card-score game card))
     (if (high-preservation-trump? game card)
       (:team-ev-high-trump-spend-penalty config)
       0)))

(defn team-ev-lead-breakdown [config game player analyses unseen-counts card]
  (let [lead (rules/effective-suit card (:trump game))
        risk (card-risk analyses card)
        partner-ruff (partner-ruff-probability-for-lead config
                                                        game
                                                        player
                                                        unseen-counts
                                                        lead)
        opponent-ruff (opponent-ruff-probability-for-lead config
                                                          game
                                                          player
                                                          unseen-counts
                                                          lead)
        team-win-prob (min 1.0 (combined-probability [(- 1.0 risk)
                                                      partner-ruff]))
        safe? (zero? risk)
        value (- (+ (* (:team-ev-trick-weight config) team-win-prob)
                    (* (:team-ev-partner-ruff-weight config) partner-ruff)
                    (if safe? (:team-ev-safe-card-bonus config) 0))
                 (* (:team-ev-risk-penalty config) risk)
                 (* (:team-ev-opponent-ruff-penalty config) opponent-ruff)
                 (card-spend-cost config game card))]
    {:card card
     :lead lead
     :risk risk
     :team-win-prob team-win-prob
     :partner-ruff-prob partner-ruff
     :opponent-ruff-prob opponent-ruff
     :spend-cost (card-spend-cost config game card)
     :value value}))

(defn team-ev-lead-value [config game player analyses unseen-counts card]
  (:value (team-ev-lead-breakdown config game player analyses unseen-counts card)))

(defn team-ev-probability-lead-card [config game player analyses cards]
  (let [unseen-counts (unseen-card-counts game player)]
    (best-lead-by-value #(team-ev-lead-value config
                                             game
                                             player
                                             analyses
                                             unseen-counts
                                             %)
                        cards)))

(defn karbosh-caller-lead-card [config game player analyses cards]
  (let [trumps (filter #(trump-card? (:trump game) %) cards)]
    (if (seq trumps)
      (first (sort-by #(risk-adjusted-lead-value config game analyses %)
                      >
                      trumps))
      (probability-lead-card config game player analyses cards))))

(defn probability-winning-card [config game _player analyses _cards winning-cards]
  (let [safe (safe-cards config analyses :win-risk-tolerance winning-cards)]
    (if (seq safe)
      (lowest-card game safe)
      (first (sort-by #(risk-adjusted-win-cost config game analyses %)
                      winning-cards)))))

(defn unsafe-high-trump-winner? [config game analyses card]
  (and (high-preservation-trump? game card)
       (> (card-risk analyses card)
          (:win-risk-tolerance config))))

(defn lower-preservation-winners [config game analyses winning-cards]
  (let [unsafe-highs (filter #(unsafe-high-trump-winner?
                                config
                                game
                                analyses
                                %)
                             winning-cards)]
    (when (seq unsafe-highs)
      (seq (remove #(some #{%} unsafe-highs) winning-cards)))))

(defn preservation-winning-card
  [config game player analyses cards winning-cards]
  (let [safe (safe-cards config analyses :win-risk-tolerance winning-cards)
        lower-winners (lower-preservation-winners config
                                                  game
                                                  analyses
                                                  winning-cards)
        non-winning (seq (remove #(wins-trick? game player %) cards))]
    (cond
      (seq safe)
      (lowest-card game safe)

      lower-winners
      (lowest-card game lower-winners)

      (and non-winning
           (seq (pending-partners-after game player))
           (every? #(unsafe-high-trump-winner? config game analyses %)
                   winning-cards))
      (lowest-card game non-winning)

      :else
      (first (sort-by #(risk-adjusted-win-cost config game analyses %)
                      winning-cards)))))

(defn probability-card-action-with-lead
  ([lead-card-fn game player]
   (probability-card-action-with-lead lead-card-fn
                                      probability-winning-card
                                      game
                                      player))
  ([lead-card-fn winning-card-fn game player]
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
                (partner-preserving-card config game player unseen-counts cards)

                (seq winning-cards)
                (winning-card-fn config game player analyses cards winning-cards)

                :else
                (ditch-card config game player unseen-counts cards))]
     (when card
       {:type :play-card
        :card card}))))

(defn threshold-probability-card-action [game player]
  (probability-card-action-with-lead threshold-lead-card game player))

(defn probability-card-action [game player]
  (probability-card-action-with-lead probability-lead-card game player))

(defn defender-exit-probability-card-action [game player]
  (probability-card-action-with-lead defender-exit-probability-lead-card
                                     game
                                     player))

(defn preservation-probability-card-action [game player]
  (probability-card-action-with-lead preservation-probability-lead-card
                                     preservation-winning-card
                                     game
                                     player))

(defn ruff-invite-preservation-card-action [game player]
  (probability-card-action-with-lead ruff-invite-preservation-lead-card
                                     preservation-winning-card
                                     game
                                     player))

(defn team-ev-probability-card-action [game player]
  (probability-card-action-with-lead team-ev-probability-lead-card
                                     preservation-winning-card
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

(defn preservation-hybrid-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (preservation-probability-card-action game player)))

(defn ruff-invite-hybrid-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (ruff-invite-preservation-card-action game player)))

(defn team-ev-hybrid-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (team-ev-probability-card-action game player)))

(def play-strategies
  {:card-counting card-counting-card-action
   :probability-threshold threshold-probability-card-action
   :probability probability-card-action
   :probability-defender-exit defender-exit-probability-card-action
   :probability-preservation preservation-probability-card-action
   :probability-ruff-invite ruff-invite-preservation-card-action
   :probability-team-ev team-ev-probability-card-action
   :hybrid-threshold hybrid-threshold-card-action
   :hybrid hybrid-card-action
   :hybrid-defender-exit defender-exit-hybrid-card-action
   :hybrid-preservation preservation-hybrid-card-action
   :hybrid-ruff-invite ruff-invite-hybrid-card-action
   :hybrid-team-ev team-ev-hybrid-card-action})

(defn resolve-play-strategy [strategy]
  (cond
    (fn? strategy) strategy
    (keyword? strategy) (or (get play-strategies strategy)
                            (throw (ex-info "Unknown play strategy"
                                            {:strategy strategy
                                             :available (keys play-strategies)})))
    :else (throw (ex-info "Invalid play strategy" {:strategy strategy}))))

(defn strategy-key [strategy fallback]
  (if (keyword? strategy) strategy fallback))

(def hybrid-engines
  {:hybrid-threshold :probability-threshold
   :hybrid :probability
   :hybrid-defender-exit :probability-defender-exit
   :hybrid-preservation :probability-preservation
   :hybrid-ruff-invite :probability-ruff-invite
   :hybrid-team-ev :probability-team-ev})

(defn card-engine [game strategy]
  (let [strategy (strategy-key strategy :custom)]
    (if (and (contains? hybrid-engines strategy)
             (special-contract? (game/current-bid game)))
      :card-counting
      (get hybrid-engines strategy strategy))))

(defn candidate-summary [game player analyses card]
  (let [{:keys [higher-unseen
                higher-follow-unseen
                higher-trump-unseen
                prob-pending-opponent-has-higher-card
                prob-pending-opponent-has-higher-follow-card
                prob-pending-opponent-void-and-higher-trump
                prob-pending-opponent-can-beat-card]} (get analyses card)]
    {:card card
     :score (card-score game card)
     :risk (round-probability (card-risk analyses card))
     :good? (zero? (card-risk analyses card))
     :trump? (trump-card? (:trump game) card)
     :winning? (wins-trick? game player card)
     :hypergeom {:higher-unseen higher-unseen
                 :higher-follow-unseen higher-follow-unseen
                 :higher-trump-unseen higher-trump-unseen
                 :prob-any-higher (round-probability
                                   prob-pending-opponent-has-higher-card)
                 :prob-higher-follow (round-probability
                                      prob-pending-opponent-has-higher-follow-card)
                 :prob-void-higher-trump-by-player
                 (into {}
                       (map (fn [[player p]]
                              [player (round-probability p)]))
                       prob-pending-opponent-void-and-higher-trump)
                 :prob-can-beat (round-probability
                                 prob-pending-opponent-can-beat-card)}}))

(defn card-reason [game player engine cards analyses card]
  (let [winner (current-trick-winner game)
        leading? (empty? (:current-trick game))
        partner-winning? (same-team? game player winner)
        selected-risk (card-risk analyses card)
        winning? (wins-trick? game player card)
        good? (zero? selected-risk)
        config (context-play-config *play-config* game player)
        unseen-counts (unseen-card-counts game player)
        winning-cards (filter #(wins-trick? game player %) cards)
        lower-winners (lower-preservation-winners config
                                                   game
                                                   analyses
                                                   winning-cards)
        ruff-invite (partner-ruff-invite-card config
                                              game
                                              player
                                              unseen-counts
                                              cards)
        defender-low-exit (defender-low-exit-card game player cards)
        trump-lead (preservation-trump-lead-card config game analyses cards)]
    (cond
      leading?
      (cond
        (= card ruff-invite)
        :partner-ruff-invite

        (= card defender-low-exit)
        :defender-low-exit

        (= card trump-lead)
        :lead-preserve-high-trump-winner

        (and (special-contract-caller? game player)
             (trump-card? (:trump game) card))
        :karbosh-caller-trump-control

        good?
        :lead-safe-card

        (#{:probability-preservation
           :probability-ruff-invite
           :hybrid-preservation
           :hybrid-ruff-invite} engine)
        :lead-preserve-high-trump

        (= :probability-team-ev engine)
        :lead-team-ev

        :else
        :lead-risk-adjusted-card)

      partner-winning?
      (if winning?
        :protect-partner-trick
        :preserve-partner-trick)

      (and winning? good?)
      :secure-winning-card

      (and winning?
           (seq lower-winners)
           (= card (lowest-card game lower-winners)))
      :preserve-high-trump-winner

      winning?
      :risk-adjusted-winning-card

      :else
      (if (= card (ditch-card config game player unseen-counts cards))
        :strategic-ditch
        :cannot-win-lowest-card))))

(defn explain-card-action [game player strategy event]
  (let [strategy (strategy-key strategy :custom)
        engine (card-engine game strategy)
        cards (vec (legal-cards game player))
        analyses (card-analyses game player cards)
        card (:card event)]
    {:source :ai
     :phase :trick-playing
     :policy strategy
     :engine engine
     :ditch-policy (:ditch-policy *play-config* default-ditch-policy)
     :reason (card-reason game player engine cards analyses card)
     :legal-count (count cards)
     :selected (candidate-summary game player analyses card)
     :candidates (mapv #(candidate-summary game player analyses %) cards)}))

(defn explain-bid-action [strategy event]
  {:source :ai
   :phase :bidding
   :policy (strategy-key strategy :custom)
   :engine :bidding
   :reason (case (:bid-type event)
             :pass :bid-pass
             :bid :numeric-contract
             :karbosh :karbosh-contract
             :double-karbosh :double-karbosh-contract
             :bid-decision)
   :selected (select-keys event [:bid-type :value])})

(defn explain-trump-action [game player event]
  (let [hand (get-in game [:players player :hand])
        strengths (into {}
                        (map (fn [suit]
                               [suit (suit-strength hand suit)]))
                        cards/suits)]
    {:source :ai
     :phase :trump-selection
     :policy :best-trump
     :engine :trump-strength
     :reason :strongest-suit
     :selected (:suit event)
     :suit-strengths strengths}))

(defn explain-donation-action [game event]
  {:source :ai
   :phase :karbosh-donation
   :policy :donate-highest-card
   :engine :card-strength
   :reason :donate-strongest-card
   :selected {:card (:card event)
              :score (card-score game (:card event))}})

(defn explain-discard-action [game event]
  {:source :ai
   :phase :karbosh-discard
   :policy :discard-lowest-card
   :engine :card-strength
   :reason :discard-weakest-card
   :selected {:card (:card event)
              :score (card-score game (:card event))}})

(defn explain-action [game player event]
  (case (:phase game)
    :bidding (explain-bid-action *bid-strategy* event)
    :trump-selection (explain-trump-action game player event)
    :karbosh-donation (explain-donation-action game event)
    :karbosh-discard (explain-discard-action game event)
    :trick-playing (explain-card-action game player *play-strategy* event)
    {:source :ai
     :phase (:phase game)
     :policy :unknown
     :reason :unknown}))

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

(defn explained-action [game player]
  (when-let [event (action game player)]
    (assoc event :ai (explain-action game player event))))
