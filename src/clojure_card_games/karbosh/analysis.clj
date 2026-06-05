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

(defn void-and-trump-probabilities
  [game player suit trump-left counts population-size players]
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
            (opponent-players game player players)))))

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
         pending-opponents (opponent-players game
                                             player
                                             (pending-trick-players-after game player))
         pending-opponent-sizes (vals (hand-sizes game pending-opponents))
         higher-count (higher-card-count unseen trump lead card)
         higher-follow-count (higher-follow-card-count unseen trump lead card)
         higher-trumps (higher-trump-count unseen trump lead card)
         higher-follow-prob (probability-of-any-success higher-follow-count
                                                        population-size
                                                        pending-opponent-sizes)
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
                                  (vals void-higher-trump-probs))]
     {:card card
      :effective-suit (effective-suit trump card)
      :lead lead
      :higher-unseen higher-count
      :higher-follow-unseen higher-follow-count
      :higher-trump-unseen higher-trumps
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
                                    void-higher-trump-prob])})))

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
