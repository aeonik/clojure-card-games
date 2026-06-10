(ns clojure-card-games.karbosh.bot.inference
  "Hidden-hand inference for the bot.

  Three evidence tiers feed these estimates, in decreasing reliability:

  1. Known voids: a player failed to follow suit, an exact public fact.
  2. Soft discard inference: throwing away a valuable off-suit card often
     means the player lacked a useful trump ruff.
  3. Action inference: a player who wins with an expensive card when a
     cheaper same-outcome card existed probably lacked the cheaper card.

  Tiers 2 and 3 never change legality or exact card-count facts; they only
  adjust probabilities that policy code may consult."
  (:require [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.karbosh.bot.cards :as bot-cards]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

;; ---------------------------------------------------------------------------
;; Known voids (exact public facts)
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Unseen-card counting
;; ---------------------------------------------------------------------------

(defn unseen-effective-suit-count [unseen-counts trump suit]
  (reduce-kv (fn [n card count]
               (if (= suit (rules/effective-suit card trump))
                 (+ n count)
                 n))
             0
             unseen-counts))

(defn total-unseen-count [unseen-counts]
  (reduce + (vals unseen-counts)))

(defn prob-hand-has-success [successes population-size hand-size]
  (if (and (pos? successes)
           (pos? hand-size)
           (<= hand-size population-size))
    (double (or (analysis/probability-of-any-success successes
                                                     population-size
                                                     [hand-size])
                0))
    0.0))

(defn combined-probability [probabilities]
  (analysis/combine-event-probabilities probabilities))

;; ---------------------------------------------------------------------------
;; Soft discard inference
;; ---------------------------------------------------------------------------

(defn soft-void-discard-confidence
  "Estimate whether a discard suggests the player was also void in `target-suit`.

  This is deliberately softer than `known-voids`: it never changes legality or
  exact card-count facts. It only lets policy code reason that throwing away a
  valuable off-suit card often means the player lacked a useful trump ruff."
  [game target-suit card]
  (let [trump (:trump game)
        effective (rules/effective-suit card trump)
        score (bot-cards/potential-card-score game card)]
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

(defn discard-void-confidence [game voids player suit]
  (if (known-void? voids player suit)
    1.0
    (analysis/combine-event-probabilities
     (soft-void-evidence game player suit))))

;; ---------------------------------------------------------------------------
;; Action inference
;; ---------------------------------------------------------------------------

(defn trick-play-prefixes [game]
  (mapcat (fn [trick]
            (let [plays (vec trick)]
              (map-indexed (fn [index play]
                             {:prefix (subvec plays 0 index)
                              :play play})
                           plays)))
          (completed-and-current-tricks game)))

(defn winning-team-after [game trump trick]
  (some->> (rules/resolve-trick trick trump)
           (game/player-team game)))

(defn same-team-outcome? [game trump left-trick right-trick]
  (= (winning-team-after game trump left-trick)
     (winning-team-after game trump right-trick)))

(defn cheaper-same-outcome-cards [game prefix player observed-card target-suit]
  (let [trump (:trump game)
        prefix (vec prefix)
        lead (rules/trick-lead prefix trump)
        observed-suit (rules/effective-suit observed-card trump)
        observed-value (when lead
                         (rules/card-value observed-card trump lead))
        observed-trick (conj prefix {:player player :card observed-card})]
    (when (and lead
               (= target-suit observed-suit)
               (winning-team-after game trump observed-trick))
      (->> (distinct (cards/deck))
           (filter #(= target-suit (rules/effective-suit % trump)))
           (filter #(< (rules/card-value % trump lead) observed-value))
           (filter #(same-team-outcome?
                     game
                     trump
                     observed-trick
                     (conj prefix {:player player :card %})))))))

(defn action-inference-evidence [config game target-player target-suit]
  (keep (fn [{:keys [prefix play]}]
          (when (and (seq prefix)
                     (= target-player (:player play)))
            (let [excluded (set (cheaper-same-outcome-cards
                                 game
                                 prefix
                                 target-player
                                 (:card play)
                                 target-suit))]
              (when (seq excluded)
                {:confidence (:action-inference-confidence config 1.0)
                 :excluded-cards excluded
                 :observed-card (:card play)}))))
        (trick-play-prefixes game)))

(defn excluded-card-count [unseen-counts cards]
  (reduce + (map #(get unseen-counts % 0) cards)))

(defn action-conditioned-suit-availability
  [config game unseen-counts target-player target-suit]
  (let [trump (:trump game)
        hand-size (count (get-in game [:players target-player :hand]))
        population-size (total-unseen-count unseen-counts)
        suit-left (unseen-effective-suit-count unseen-counts trump target-suit)
        base (prob-hand-has-success suit-left population-size hand-size)
        evidence (vec (action-inference-evidence
                       config
                       game
                       target-player
                       target-suit))
        confidence (analysis/combine-event-probabilities
                    (map :confidence evidence))
        excluded-cards (set (mapcat :excluded-cards evidence))
        excluded-total (excluded-card-count unseen-counts excluded-cards)
        excluded-suit (excluded-card-count
                       unseen-counts
                       (filter #(= target-suit
                                   (rules/effective-suit % trump))
                               excluded-cards))
        conditioned (prob-hand-has-success
                     (max 0 (- suit-left excluded-suit))
                     (max 0 (- population-size excluded-total))
                     hand-size)]
    (+ (* (- 1.0 confidence) base)
       (* confidence conditioned))))

;; ---------------------------------------------------------------------------
;; Combined suit availability
;; ---------------------------------------------------------------------------

(defn suit-void-probability [config game _observer unseen-counts voids player suit]
  (if (known-void? voids player suit)
    1.0
    (let [discard (discard-void-confidence game voids player suit)
          available (action-conditioned-suit-availability
                     config
                     game
                     unseen-counts
                     player
                     suit)]
      (analysis/combine-event-probabilities
       [discard (- 1.0 available)]))))

(defn suit-available-probability [config game observer unseen-counts voids player suit]
  (- 1.0
     (suit-void-probability config game observer unseen-counts voids player suit)))

(defn soft-void-confidence
  ([game voids player suit]
   (discard-void-confidence game voids player suit))
  ([config game observer unseen-counts voids player suit]
   (suit-void-probability config game observer unseen-counts voids player suit)))

(defn likely-void? [config game observer unseen-counts voids player suit]
  (>= (soft-void-confidence config
                            game
                            observer
                            unseen-counts
                            voids
                            player
                            suit)
      (:soft-void-trump-threshold config 0.65)))

(defn opponents-likely-void-in-suit? [config game observer unseen-counts voids player suit]
  (every? #(likely-void? config game observer unseen-counts voids % suit)
          (bot-cards/players-with-cards
           game
           (remove #(bot-cards/same-team? game player %) (game/trick-players game)))))

(defn partners-known-void-in-suit? [game voids player suit]
  (let [partners (bot-cards/players-with-cards game (game/partner-players game player))]
    (and (seq partners)
         (every? #(known-void? voids % suit) partners))))

;; ---------------------------------------------------------------------------
;; Ruff probabilities
;; ---------------------------------------------------------------------------

(defn prob-void-and-trump-for-player
  [config game player unseen-counts voids lead other]
  (let [trump (:trump game)
        population-size (total-unseen-count unseen-counts)
        hand-size (count (get-in game [:players other :hand]))
        trump-left (unseen-effective-suit-count unseen-counts trump trump)
        lead-left (unseen-effective-suit-count unseen-counts trump lead)
        trump-available-prob (suit-available-probability config
                                                         game
                                                         player
                                                         unseen-counts
                                                         voids
                                                         other
                                                         trump)]
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
         (bot-cards/probability
          (analysis/probability-specific-void-and-trump
           lead-left
           trump-left
           population-size
           hand-size))))))

(defn partner-ruff-probability-for-lead
  [config game player unseen-counts lead]
  (let [voids (known-voids game)]
    (combined-probability
     (map #(prob-void-and-trump-for-player config
                                           game
                                           player
                                           unseen-counts
                                           voids
                                           lead
                                           %)
          (bot-cards/players-with-cards
           game
           (bot-cards/pending-partners-after game player))))))

(defn opponent-ruff-probability-for-lead
  [config game player unseen-counts lead]
  (let [voids (known-voids game)]
    (combined-probability
     (map #(prob-void-and-trump-for-player config
                                           game
                                           player
                                           unseen-counts
                                           voids
                                           lead
                                           %)
          (bot-cards/players-with-cards
           game
           (bot-cards/pending-opponents-after game player))))))
