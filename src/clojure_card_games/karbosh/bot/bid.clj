(ns clojure-card-games.karbosh.bot.bid
  "Bidding policy: hand features, Karbosh/double-Karbosh evaluation,
  donation analysis, and numeric bid targets.

  Every function takes its bid config explicitly; the dynamic strategy and
  config selection lives in `clojure-card-games.karbosh.bot`."
  (:require [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.karbosh.bot.cards :as bot-cards]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.probability.hypergeom :as hypergeom]))

(defn suit-strength [hand suit]
  (reduce + (map #(rules/card-value % suit suit) hand)))

(defn best-trump [hand]
  (apply max-key #(suit-strength hand %) cards/suits))

;; ---------------------------------------------------------------------------
;; Donation analysis
;; ---------------------------------------------------------------------------

(defn helpful-donation-card? [config trump card]
  (or (bot-cards/bower? trump card)
      (bot-cards/high-trump? config trump card)
      (bot-cards/off-ace? trump card)))

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
        discards (bot-cards/karbosh-discard-cards hand trump)
        kept-hand (bot-cards/remove-cards hand discards)
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

;; ---------------------------------------------------------------------------
;; Karbosh hand features
;; ---------------------------------------------------------------------------

(defn karbosh-hand? [config hand trump]
  (let [trumps (filter #(bot-cards/trump-card? trump %) hand)
        high-trumps (filter #(bot-cards/high-trump? config trump %) trumps)
        bowers (filter #(bot-cards/bower? trump %) trumps)
        winners (+ (count high-trumps)
                   (count (filter #(bot-cards/off-ace? trump %) hand)))]
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
      (and (bot-cards/trump-card? trump card) (= :A rank)) 3
      (and (bot-cards/trump-card? trump card) (= :K rank)) 4
      (and (bot-cards/trump-card? trump card) (= :Q rank)) 5
      (and (bot-cards/trump-card? trump card) (= 10 rank)) 6
      (and (bot-cards/trump-card? trump card) (= 9 rank)) 7
      :else 99)))

(defn top-trump-prefix-count [hand trump]
  (loop [needed top-trump-ranks
         actual (sort (map #(trump-control-rank trump %)
                           (filter #(bot-cards/trump-card? trump %) hand)))
         prefix 0]
    (if (and (seq needed)
             (seq actual)
             (<= (first actual) (first needed)))
      (recur (rest needed) (rest actual) (inc prefix))
      prefix)))

(defn double-karbosh-features [hand trump]
  (let [hand (vec hand)
        trumps (filter #(bot-cards/trump-card? trump %) hand)
        prefix (top-trump-prefix-count hand trump)
        off-aces (filter #(bot-cards/off-ace? trump %) hand)
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

;; ---------------------------------------------------------------------------
;; Numeric bids
;; ---------------------------------------------------------------------------

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
        trumps (filter #(bot-cards/trump-card? trump %) hand)
        high-trumps (filter #(bot-cards/high-trump? config trump %) trumps)
        bowers (filter #(bot-cards/bower? trump %) trumps)
        off-aces (filter #(bot-cards/off-ace? trump %) hand)
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

(defn target-bid [config hand]
  (let [hand (vec hand)
        trump (best-trump hand)]
    (if (karbosh-hand? config hand trump)
      {:type :bid :bid-type :karbosh}
      (numeric-target-bid config hand))))

;; ---------------------------------------------------------------------------
;; Probability-based Karbosh evaluation
;; ---------------------------------------------------------------------------

(defn sigmoid [x]
  (/ 1.0 (+ 1.0 (Math/exp (- x)))))

(defn karbosh-features [config hand trump]
  (let [trumps (filter #(bot-cards/trump-card? trump %) hand)
        high-trumps (filter #(bot-cards/high-trump? config trump %) trumps)
        right-bowers (filter #(rules/right-bower? % trump) trumps)
        left-bowers (filter #(rules/left-bower? % trump) trumps)
        bowers (concat right-bowers left-bowers)
        off-cards (remove #(bot-cards/trump-card? trump %) hand)
        off-aces (filter #(bot-cards/off-ace? trump %) off-cards)
        low-trumps (remove #(bot-cards/high-trump? config trump %) trumps)
        off-junk (remove #(bot-cards/off-ace? trump %) off-cards)]
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

;; ---------------------------------------------------------------------------
;; Score context
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Contract evaluation
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Bid candidates
;; ---------------------------------------------------------------------------

(defn threshold-bid-candidate [config game player]
  (target-bid config (get-in game [:players player :hand])))

(defn playable-bid [game candidate]
  (let [current-rank (rules/bid-rank (game/current-bid game))]
    (if (> (rules/bid-rank candidate) current-rank)
      candidate
      {:type :bid :bid-type :pass})))

(defn probability-bid-candidate [numeric-target-fn config game player]
  (let [hand (get-in game [:players player :hand])
        trump (best-trump hand)
        double-karbosh (double-karbosh-evaluation config game player trump)
        karbosh (karbosh-evaluation config game player trump)]
    (cond
      (:call? double-karbosh)
      {:type :bid :bid-type :double-karbosh}

      (:call? karbosh)
      {:type :bid :bid-type :karbosh}

      :else
      (numeric-target-fn config hand))))

(defn threshold-bid-action [config game player]
  (playable-bid game (threshold-bid-candidate config game player)))

(defn probability-bid-action [config game player]
  (playable-bid game
                (probability-bid-candidate numeric-target-bid config game player)))

(defn conservative-probability-bid-action [config game player]
  (playable-bid game
                (probability-bid-candidate conservative-numeric-target-bid
                                           config
                                           game
                                           player)))
