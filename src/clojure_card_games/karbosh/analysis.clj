(ns clojure-card-games.karbosh.analysis
  (:require [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.probability.hypergeom :as hypergeom]))

(defn completed-trick-cards [game]
  (mapcat (fn [trick]
            (map :card trick))
          (:completed-tricks game)))

(defn current-trick-cards [game]
  (map :card (:current-trick game)))

(defn completed-and-current-tricks [game]
  (cond-> (vec (:completed-tricks game))
    (seq (:current-trick game)) (conj (:current-trick game))))

(defn trick-known-voids
  "Hard public void facts from one trick.

  If a player fails to follow the effective lead suit, then every future exact
  probability calculation may condition on that player holding zero cards in
  that suit. Softer discard/action inference lives in `bot.inference` and does
  not feed these exact combinatorics."
  [trump trick]
  (let [lead (rules/trick-lead trick trump)]
    (when lead
      (keep (fn [{:keys [player card]}]
              (when (not= lead (rules/effective-suit card trump))
                [player lead]))
            trick))))

(defn known-voids
  "Exact public voids, including the current trick prefix when present."
  [game]
  (let [trump (:trump game)]
    (reduce (fn [voids [player suit]]
              (update voids player (fnil conj #{}) suit))
            {}
            (mapcat #(trick-known-voids trump %)
                    (completed-and-current-tricks game)))))

(defn public-played-cards [game]
  (concat (completed-trick-cards game)
          (current-trick-cards game)))

(defn seen-cards
  "Cards known to `player`: its current hand plus public trick cards."
  [game player]
  (concat (get-in game [:players player :hand])
          (public-played-cards game)))

(defn remove-seen [deck seen]
  (reduce (fn [deck card]
            (game/remove-first card deck))
          deck
          seen))

(defn unseen-cards [game player]
  (remove-seen (cards/deck) (seen-cards game player)))

(defn unseen-card-counts [game player]
  (frequencies (unseen-cards game player)))

(defn effective-suit [trump card]
  (if trump
    (rules/effective-suit card trump)
    (second card)))

(defn effective-suit-counts [trump cards]
  (merge (zipmap cards/suits (repeat 0))
         (frequencies (map #(effective-suit trump %) cards))))

(defn player-hand-size [game player]
  (count (get-in game [:players player :hand])))

(defn hand-sizes [game players]
  (into {}
        (map (fn [player]
               [player (player-hand-size game player)]))
        players))

(defn opponent? [game player other]
  (not= (game/player-team game player)
        (game/player-team game other)))

(defn opponent-players [game player players]
  (filterv #(opponent? game player %) players))

(defn hidden-players [player]
  (filterv #(not= player %) game/players))

(defn active-hidden-players [game player]
  (filterv #(not= player %) (game/trick-players game)))

(defn pending-trick-players-after
  "Players still able to act after `player` plays into the current trick."
  [game player]
  (let [played (conj (set (map :player (:current-trick game))) player)]
    (filterv #(not (contains? played %)) (game/trick-players game))))

(defn probability-of-any-success
  "Probability that the labeled hands collectively contain at least one success."
  [successes population-size hand-sizes]
  (let [draws (reduce + hand-sizes)
        failures (- population-size successes)]
    (cond
      (or (not (pos? successes))
          (not (pos? draws)))
      0

      (or (neg? failures)
          (> draws population-size))
      0

      :else
      (hypergeom/tail-geq successes failures draws 1))))

(defn prob-labeled-hand-sizes-min-success
  "Probability every hand in `hand-sizes` has at least `min-success` successes.

  This is the variable-hand-size version of
  `hypergeom/prob-labeled-hands-min-success`, which matters mid-trick."
  [successes failures hand-sizes min-success]
  (cond
    (or (neg? successes)
        (neg? failures)
        (some neg? hand-sizes)
        (neg? min-success))
    0

    (empty? hand-sizes)
    1

    (< (+ successes failures) (reduce + hand-sizes))
    0

    :else
    (let [hand-size (first hand-sizes)]
      (reduce +
              (for [k (range min-success (inc (min hand-size successes)))
                    :let [failure-count (- hand-size k)]
                    :when (<= 0 failure-count failures)]
                (* (hypergeom/prob-hg successes failures hand-size k)
                   (prob-labeled-hand-sizes-min-success
                     (- successes k)
                     (- failures failure-count)
                     (rest hand-sizes)
                     min-success)))))))

(defn successful-hand-count-distribution
  "Probability distribution for how many labeled hands have at least
  `min-success` successes.

  The result maps successful-hand-count to exact probability. This is useful for
  donation questions where each partner may donate at most one useful card."
  [successes failures hand-sizes min-success]
  (cond
    (or (neg? successes)
        (neg? failures)
        (some neg? hand-sizes)
        (neg? min-success))
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
                         child (successful-hand-count-distribution
                                 (- successes k)
                                 (- failures failure-count)
                                 (rest hand-sizes)
                                 min-success)
                         successful? (>= k min-success)]]
               (into {}
                     (map (fn [[n child-p]]
                            [(+ n (if successful? 1 0)) (* p child-p)]))
                     child))))))

(defn probability-at-least-successful-hands
  "Probability that at least `min-hands` labeled hands each contain at least
  `min-success` successes."
  ([successes failures hand-sizes min-hands]
   (probability-at-least-successful-hands successes
                                          failures
                                          hand-sizes
                                          min-hands
                                          1))
  ([successes failures hand-sizes min-hands min-success]
   (reduce +
           (for [[successful-hands p]
                 (successful-hand-count-distribution successes
                                                     failures
                                                     hand-sizes
                                                     min-success)
                 :when (>= successful-hands min-hands)]
             p))))

(defn probability-all-follow [suit-left population-size hand-sizes]
  (let [failures (- population-size suit-left)]
    (if (neg? failures)
      0
      (prob-labeled-hand-sizes-min-success suit-left failures hand-sizes 1))))

(defn probability-specific-void-and-trump
  [suit-left trump-left population-size hand-size]
  (cond
    (or (not (pos? hand-size))
        (neg? suit-left)
        (neg? trump-left)
        (> (+ suit-left trump-left) population-size))
    0

    :else
    (hypergeom/prob-void-and-trump suit-left
                                   trump-left
                                   population-size
                                   hand-size)))

(defn higher-card-count [unseen-cards trump lead card]
  (count (filter #(rules/beats? trump lead % card) unseen-cards)))

(defn higher-follow-card-count [unseen-cards trump lead card]
  (count (filter #(and (= lead (effective-suit trump %))
                       (rules/beats? trump lead % card))
                 unseen-cards)))

(defn higher-trump-count [unseen-cards trump lead card]
  (if (and trump (not= lead trump))
    (count (filter #(and (= trump (effective-suit trump %))
                         (rules/beats? trump lead % card))
                   unseen-cards))
    0))

(defn lower-or-equal-follow-count
  "Cards that can follow `lead` without beating `card`.

  Equal duplicate cards are included here. The earlier equivalent card wins a
  Karbosh tie, so a later equal card can follow without taking control."
  [cards-by-suit higher-follow-count lead]
  (- (get cards-by-suit lead 0) higher-follow-count))

(defn combine-event-probabilities [probabilities]
  (- 1.0
     (reduce * 1.0 (map #(- 1.0 (double (or % 0)))
                        probabilities))))

(defn suit-analysis [population-size suit-count hand-sizes]
  {:unseen suit-count
   :prob-any-hand-has
   (probability-of-any-success suit-count population-size hand-sizes)
   :prob-all-hands-can-follow
   (probability-all-follow suit-count population-size hand-sizes)})

(defn void-and-trump-probabilities-for
  [game suit trump-left counts population-size players]
  (when (pos? trump-left)
    (let [suit-left (get counts suit 0)]
      (into {}
            (map (fn [other]
                   [other
                    (probability-specific-void-and-trump
                      suit-left
                      trump-left
                      population-size
                      (player-hand-size game other))]))
            players))))

(defn void-and-trump-probabilities
  [game player suit trump-left counts population-size players]
  (void-and-trump-probabilities-for game
                                    suit
                                    trump-left
                                    counts
                                    population-size
                                    (opponent-players game player players)))

(defn category-choice-count [high-count low-count other-count
                             high-draw low-draw other-draw]
  (*' (hypergeom/choose high-count high-draw)
      (hypergeom/choose low-count low-draw)
      (hypergeom/choose other-count other-draw)))

(defn constrained-category-deal-count
  "Count labeled hidden-hand deals by three categories.

  Categories are:
  - `high-count`: cards that follow the lead suit and beat the candidate card.
  - `low-count`: cards that follow the lead suit but do not beat the candidate.
  - `other-count`: every other hidden card.

  `voids` is a hard public-void map. A player known void in `lead` is constrained
  to draw zero high and zero low cards. When `target` is supplied, that player is
  additionally constrained to draw at least one high card and zero low cards.

  The result is a count, not a probability. Dividing two counts gives an exact
  ratio while preserving the known-void conditioning in the denominator."
  [high-count low-count other-count players hand-sizes voids lead target]
  (let [players (vec players)
        player-count (count players)
        memo* (atom {})]
    (letfn [(step [high-count low-count other-count player-idx]
              (let [memo-key [high-count low-count other-count player-idx]]
                (if-let [cached (find @memo* memo-key)]
                  (val cached)
                  (let [result
                        (if (= player-idx player-count)
                          (if (and (zero? high-count)
                                   (zero? low-count)
                                   (zero? other-count))
                            1N
                            0N)
                          (let [player (nth players player-idx)
                                hand-size (get hand-sizes player 0)
                                void? (contains? (get voids player #{}) lead)]
                            (reduce
                             +
                             (for [high-draw (if void?
                                                [0]
                                                (range 0 (inc (min hand-size
                                                                   high-count))))
                                   low-draw (if void?
                                              [0]
                                              (range 0 (inc (min (- hand-size
                                                                    high-draw)
                                                                 low-count))))
                                   :let [other-draw (- hand-size high-draw low-draw)]
                                   :when (and (<= 0 other-draw other-count)
                                              (or (not= player target)
                                                  (and (pos? high-draw)
                                                       (zero? low-draw))))
                                   :let [ways (category-choice-count high-count
                                                                     low-count
                                                                     other-count
                                                                     high-draw
                                                                     low-draw
                                                                     other-draw)]
                                   :when (pos? ways)]
                               (*' ways
                                   (step (- high-count high-draw)
                                         (- low-count low-draw)
                                         (- other-count other-draw)
                                         (inc player-idx)))))))]
                    (swap! memo* assoc memo-key result)
                    result))))]
      (step high-count low-count other-count 0))))

(defn forced-higher-follow-probability-for
  "Exact probability that `target` must burn a higher follow-suit control.

  This answers a different question from ordinary card risk. It is evaluated
  after hard void facts are known: the denominator contains only hidden deals
  where every known-void player truly has zero cards in the led suit. Under that
  conditioned distribution, `target` is forced to overtake when all are true:

  1. `target` holds at least one hidden card that follows `lead` and beats
     `card`.
  2. `target` holds no lower/equal card in `lead` that could be played instead.
  3. all public void constraints remain satisfied.

  Example: if a partner is known void in trump, the remaining left bower is
  redistributed only among players who can still hold trump. This is the exact
  downstream correction that a naive `hand-size / unseen-count` estimate misses.
  It complements the ordinary void-and-trump risk calculation: first infer who
  can or cannot hold the led suit, then ask whether leading this card burns a
  partner's higher control in those conditioned worlds."
  [game player lead card unseen cards-by-suit population-size target]
  (let [voids (known-voids game)
        players (hidden-players player)
        hand-sizes (hand-sizes game players)
        high-count (higher-follow-card-count unseen (:trump game) lead card)
        low-count (lower-or-equal-follow-count cards-by-suit high-count lead)
        other-count (- population-size high-count low-count)
        denominator (constrained-category-deal-count high-count
                                                     low-count
                                                     other-count
                                                     players
                                                     hand-sizes
                                                     voids
                                                     lead
                                                     nil)]
    (cond
      (or (zero? denominator)
          (contains? (get voids target #{}) lead))
      0

      :else
      (/ (constrained-category-deal-count high-count
                                          low-count
                                          other-count
                                          players
                                          hand-sizes
                                          voids
                                          lead
                                          target)
         denominator))))

(defn forced-higher-follow-probabilities
  [game player lead card unseen cards-by-suit population-size players]
  (into {}
        (map (fn [target]
               [target
                (forced-higher-follow-probability-for game
                                                      player
                                                      lead
                                                      card
                                                      unseen
                                                      cards-by-suit
                                                      population-size
                                                      target)]))
        players))

(defn ruff-probabilities [game player trump suit counts population-size players]
  (when (and trump (not= suit trump))
    (void-and-trump-probabilities game
                                  player
                                  suit
                                  (get counts trump 0)
                                  counts
                                  population-size
                                  players)))

(defn card-defeat-analysis
  ([game player card]
   (let [trump (:trump game)
         unseen (vec (unseen-cards game player))
         population-size (count unseen)
         counts (effective-suit-counts trump unseen)]
     (card-defeat-analysis game
                           player
                           trump
                           unseen
                           counts
                           population-size
                           card)))
  ([game player trump unseen cards-by-suit population-size card]
   (let [lead (or (rules/trick-lead (:current-trick game) trump)
                  (effective-suit trump card))
         pending-players (pending-trick-players-after game player)
         pending-opponents (opponent-players game player pending-players)
         pending-partners (filterv #(= (game/player-team game player)
                                       (game/player-team game %))
                                   pending-players)
         pending-player-sizes (vals (hand-sizes game pending-players))
         pending-opponent-sizes (vals (hand-sizes game pending-opponents))
         higher-count (higher-card-count unseen trump lead card)
         higher-follow-count (higher-follow-card-count unseen trump lead card)
         higher-trumps (higher-trump-count unseen trump lead card)
         pending-player-higher-follow-prob
         (probability-of-any-success higher-follow-count
                                     population-size
                                     pending-player-sizes)
         higher-follow-prob (probability-of-any-success higher-follow-count
                                                        population-size
                                                        pending-opponent-sizes)
         pending-player-void-higher-trump-probs
         (when (and trump (not= lead trump))
           (void-and-trump-probabilities-for game
                                             lead
                                             higher-trumps
                                             cards-by-suit
                                             population-size
                                             pending-players))
         pending-player-void-higher-trump-prob
         (combine-event-probabilities
          (vals pending-player-void-higher-trump-probs))
         void-higher-trump-probs (when (and trump (not= lead trump))
                                   (void-and-trump-probabilities
                                     game
                                     player
                                     lead
                                     higher-trumps
                                     cards-by-suit
                                     population-size
                                     pending-opponents))
         void-higher-trump-prob (combine-event-probabilities
                                  (vals void-higher-trump-probs))
         partner-forced-higher-follow-probs
         (forced-higher-follow-probabilities game
                                             player
                                             lead
                                             card
                                             unseen
                                             cards-by-suit
                                             population-size
                                             pending-partners)]
     {:card card
      :effective-suit (effective-suit trump card)
      :lead lead
      :higher-unseen higher-count
      :higher-follow-unseen higher-follow-count
      :higher-trump-unseen higher-trumps
      :prob-pending-player-has-higher-card
      (probability-of-any-success higher-count
                                  population-size
                                  pending-player-sizes)
      :prob-pending-player-has-higher-follow-card
      pending-player-higher-follow-prob
      :prob-pending-player-void-and-higher-trump
      pending-player-void-higher-trump-probs
      :prob-pending-player-can-beat-card
      (combine-event-probabilities [pending-player-higher-follow-prob
                                    pending-player-void-higher-trump-prob])
      :prob-pending-opponent-has-higher-card
      (probability-of-any-success higher-count
                                  population-size
                                  pending-opponent-sizes)
      :prob-pending-opponent-has-higher-follow-card
      higher-follow-prob
      :prob-pending-opponent-void-and-higher-trump
      void-higher-trump-probs
      :prob-pending-opponent-can-beat-card
      (combine-event-probabilities [higher-follow-prob
                                    void-higher-trump-prob])
      :prob-pending-partner-forced-higher-follow
      partner-forced-higher-follow-probs
      :expected-pending-partner-control-burn
      (reduce + (vals partner-forced-higher-follow-probs))})))

(defn card-analysis
  [game player trump unseen cards-by-suit population-size card]
  (let [{:keys [lead] :as defeat-analysis}
        (card-defeat-analysis game
                              player
                              trump
                              unseen
                              cards-by-suit
                              population-size
                              card)
        pending-opponents (opponent-players game
                                            player
                                            (pending-trick-players-after game player))
        pending-opponent-sizes (vals (hand-sizes game pending-opponents))
        suit-left (get cards-by-suit lead 0)]
    (assoc defeat-analysis
           :prob-pending-opponents-all-follow
           (probability-all-follow suit-left
                                   population-size
                                   pending-opponent-sizes)
           :prob-pending-opponent-void-and-trump
           (ruff-probabilities game
                               player
                               trump
                               lead
                               cards-by-suit
                               population-size
                               pending-opponents))))

(defn trump-analysis [game player trump]
  (let [hand (get-in game [:players player :hand])
        unseen (vec (unseen-cards game player))
        population-size (count unseen)
        counts (effective-suit-counts trump unseen)
        hidden (hidden-players player)
        active-hidden (active-hidden-players game player)
        opponents (opponent-players game player hidden)
        active-opponents (opponent-players game player active-hidden)
        opponent-sizes (vals (hand-sizes game opponents))
        active-opponent-sizes (vals (hand-sizes game active-opponents))]
    {:trump trump
     :unseen-effective-suits counts
     :opponent-hand-sizes (hand-sizes game opponents)
     :active-opponent-hand-sizes (hand-sizes game active-opponents)
     :suits
     (into {}
           (map (fn [suit]
                  [suit
                   (assoc (suit-analysis population-size
                                         (get counts suit 0)
                                         opponent-sizes)
                          :active-opponents
                          (suit-analysis population-size
                                         (get counts suit 0)
                                         active-opponent-sizes))]))
           cards/suits)
     :hand
     (mapv #(card-analysis game player trump unseen counts population-size %)
           hand)}))

(defn player-analysis
  "Derived hypergeometric view for a player from its visible information.

  No game state is mutated. During bidding this returns candidate analyses for
  all possible trump suits; after trump is known it returns `:trump-analysis`."
  [game player]
  (let [seen (vec (seen-cards game player))
        unseen (vec (unseen-cards game player))]
    (cond-> {:player player
             :phase (:phase game)
             :hand-count (player-hand-size game player)
             :seen-count (count seen)
             :unseen-count (count unseen)
             :hidden-hand-sizes (hand-sizes game (hidden-players player))
             :unseen-card-counts (frequencies unseen)}
      (:trump game)
      (assoc :trump-analysis (trump-analysis game player (:trump game)))

      (not (:trump game))
      (assoc :candidate-trumps
             (into {}
                   (map (fn [trump]
                          [trump (trump-analysis game player trump)]))
                   cards/suits)))))
