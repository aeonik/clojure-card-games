(ns clojure-card-games.karbosh.bot.play
  "Card-play policy: lead selection, winning-card selection, strategic
  ditching, and the team-EV evaluator.

  Every function takes its play config explicitly; dynamic strategy and
  config selection lives in `clojure-card-games.karbosh.bot`."
  (:require [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.karbosh.bot.cards :as bot-cards]
            [clojure-card-games.karbosh.bot.config :as config]
            [clojure-card-games.karbosh.bot.inference :as inference]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(defn context-play-config
  "Tighten risk settings when `player` is playing its own special contract."
  [play-config game player]
  (if (bot-cards/special-contract-caller? game player)
    (assoc play-config
           :lead-risk-tolerance (:karbosh-lead-risk-tolerance play-config)
           :win-risk-tolerance (:karbosh-win-risk-tolerance play-config)
           :lead-risk-penalty (:karbosh-lead-risk-penalty play-config)
           :win-risk-penalty (:karbosh-win-risk-penalty play-config))
    play-config))

;; ---------------------------------------------------------------------------
;; Lead helpers
;; ---------------------------------------------------------------------------

(defn secure-trump-lead-card [game unseen-counts cards]
  (let [trumps (filter #(bot-cards/trump-card? (:trump game) %) cards)
        secure-trumps (filter #(bot-cards/good-card-with-counts? game unseen-counts %)
                              trumps)]
    (when (seq secure-trumps)
      (bot-cards/highest-card game secure-trumps))))

(defn caller-pressure-lead-card [game cards]
  (or (when-let [off-aces (seq (filter #(bot-cards/off-ace? (:trump game) %) cards))]
        (bot-cards/lowest-card game off-aces))
      (when-let [low-trumps (seq (filter #(bot-cards/low-trump? (:trump game) %) cards))]
        (bot-cards/lowest-card game low-trumps))))

;; ---------------------------------------------------------------------------
;; Strategic ditching
;; ---------------------------------------------------------------------------

(defn higher-follow-count-with-counts [game unseen-counts card]
  (let [trump (:trump game)
        lead (bot-cards/card-effective-suit game card)]
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
  (if (bot-cards/trump-card? (:trump game) card)
    0.0
    (* (bot-cards/potential-card-score game card)
       (future-suit-control-probability game player unseen-counts card))))

(defn best-same-suit-equity-after-discard [game player unseen-counts card]
  (let [suit (bot-cards/card-effective-suit game card)]
    (reduce max
            0.0
            (map #(if (= suit (bot-cards/card-effective-suit game %))
                    (future-suit-equity game player unseen-counts %)
                    0.0)
                 (bot-cards/remaining-hand-after game player card)))))

(defn future-suit-equity-loss [game player unseen-counts card]
  (max 0.0
       (- (future-suit-equity game player unseen-counts card)
          (best-same-suit-equity-after-discard
            game
            player
            unseen-counts
            card))))

(defn ditch-future-suit-equity-weight [play-config]
  (case (:ditch-policy play-config config/default-ditch-policy)
    :classic 0
    :future-suit-equity (:ditch-future-suit-equity-weight play-config 50)
    (:ditch-future-suit-equity-weight play-config 50)))

(defn ditch-card-cost [play-config game player unseen-counts cards card]
  (let [trump (:trump game)
        hand (get-in game [:players player :hand])
        counts (bot-cards/suit-counts game hand)
        suit (bot-cards/card-effective-suit game card)
        suit-count (get counts suit 0)
        remaining-suit-count (max 0 (dec suit-count))
        trump? (bot-cards/trump-card? trump card)
        has-trump? (some #(bot-cards/trump-card? trump %) hand)
        non-trump-legal? (some #(not (bot-cards/trump-card? trump %)) cards)
        control? (bot-cards/in-suit-control-card? game unseen-counts card)
        last-control? (and control?
                           (not (bot-cards/same-suit-control-after-discard?
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
    (- (+ (bot-cards/potential-card-score game card)
          (if (and trump? non-trump-legal?) 5000 0)
          (if last-control? 3000 0)
          (* (ditch-future-suit-equity-weight play-config)
             future-equity-loss)
          (* 8 suit-count))
       short-suit-bonus)))

(defn ditch-card
  "Choose a card to throw away when this play is not trying to win the trick.

  The ranking preserves trump, exact suit controls, and future suit equity
  estimated from unseen cards, while using low off-suit cards to clear suits
  when the player has trump left for future ruffs."
  ([game player unseen-counts cards]
   (ditch-card config/default-play-config game player unseen-counts cards))
  ([play-config game player unseen-counts cards]
   (first (sort-by #(ditch-card-cost play-config game player unseen-counts cards %)
                   cards))))

;; ---------------------------------------------------------------------------
;; Partner support
;; ---------------------------------------------------------------------------

(defn suit-protecting-card? [game player unseen-counts lead card]
  (and (= lead (rules/effective-suit card (:trump game)))
       (bot-cards/wins-trick? game player card)
       (not (bot-cards/can-be-beaten-in-suit-by? game unseen-counts card lead))))

(defn partner-protecting-card [game player unseen-counts cards]
  (let [lead (rules/trick-lead (:current-trick game) (:trump game))
        winner-card (:card (rules/winning-play (:current-trick game)
                                               (:trump game)))
        vulnerable? (and lead
                         (seq (bot-cards/pending-opponents-after game player))
                         (bot-cards/can-be-beaten-in-suit-by? game
                                                              unseen-counts
                                                              winner-card
                                                              lead))
        protectors (filter #(suit-protecting-card?
                              game player unseen-counts lead %)
                           cards)]
    (when (and vulnerable? (seq protectors))
      (bot-cards/lowest-card game protectors))))

(defn partner-preserving-card
  ([game player unseen-counts cards]
   (partner-preserving-card config/default-play-config game player unseen-counts cards))
  ([play-config game player unseen-counts cards]
   (let [non-overtakers (remove #(bot-cards/wins-trick? game player %) cards)]
     (or (partner-protecting-card game player unseen-counts cards)
         (ditch-card play-config
                     game
                     player
                     unseen-counts
                     (or (seq non-overtakers) cards))))))

(defn partner-ruff-invite-card
  ([game player unseen-counts cards]
   (partner-ruff-invite-card config/default-play-config game player unseen-counts cards))
  ([play-config game player unseen-counts cards]
   (let [trump (:trump game)
         voids (inference/known-voids game)
         secure-trump (secure-trump-lead-card game unseen-counts cards)
         unseen-trumps (inference/unseen-effective-suit-count unseen-counts trump trump)
         off-suit-cards (remove #(bot-cards/trump-card? trump %) cards)
         partner-void-cards (filter #(inference/partners-known-void-in-suit?
                                       game
                                       voids
                                       player
                                       (rules/effective-suit % trump))
                                    off-suit-cards)]
     (when (and trump
                secure-trump
                (pos? unseen-trumps)
                (inference/opponents-likely-void-in-suit? play-config
                                                          game
                                                          player
                                                          unseen-counts
                                                          voids
                                                          player
                                                          trump)
                (seq partner-void-cards))
       (bot-cards/lowest-card game partner-void-cards)))))

;; ---------------------------------------------------------------------------
;; Card-counting engine
;; ---------------------------------------------------------------------------

(defn card-counting-card-action [play-config game player]
  (let [cards (vec (bot-cards/legal-cards game player))
        winner (bot-cards/current-trick-winner game)
        config (context-play-config play-config game player)
        unseen-counts (bot-cards/unseen-card-counts game player)
        winning-cards (filter #(bot-cards/wins-trick? game player %) cards)
        good-cards (filter #(bot-cards/good-card-with-counts? game unseen-counts %) cards)
        secure-winning-cards (filter #(bot-cards/secure-winning-card-with-counts?
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
                 (bot-cards/lowest-card game good-cards)
                 (bot-cards/highest-card game cards))

               (bot-cards/same-team? game player winner)
               (partner-preserving-card config game player unseen-counts cards)

               (seq secure-winning-cards)
               (bot-cards/lowest-card game secure-winning-cards)

               (seq winning-cards)
               (bot-cards/lowest-card game winning-cards)

               :else
               (ditch-card config game player unseen-counts cards))]
    (when card
      {:type :play-card
       :card card})))

;; ---------------------------------------------------------------------------
;; Lead evaluation
;; ---------------------------------------------------------------------------

(defn lead-card-features [game analyses card]
  (let [risk (bot-cards/card-risk analyses card)]
    {:card card
     :score (bot-cards/card-score game card)
     :risk risk
     :trump? (bot-cards/trump-card? (:trump game) card)
     :high-trump? (bot-cards/high-preservation-trump? game card)
     :good? (zero? risk)}))

(defn risk-adjusted-lead-value [play-config game analyses card]
  (- (bot-cards/card-score game card)
     (* (:lead-risk-penalty play-config) (bot-cards/card-risk analyses card))))

(defn defender-preservation-penalty [play-config context features]
  (if (and (:defender? context)
           (:trump? features)
           (:high-trump? features)
           (not (:good? features)))
    (* (or (:defender-high-trump-preservation-penalty play-config) 0)
       (:risk features))
    0.0))

(defn partner-control-burn [analyses card]
  (double (or (get-in analyses [card :expected-pending-partner-control-burn])
              0)))

(defn opponent-ruff-risk [analyses card]
  (analysis/combine-event-probabilities
   (vals (or (get-in analyses [card :prob-pending-opponent-void-and-higher-trump])
             {}))))

(defn ruff-exposed-control-burn [analyses card]
  (* (partner-control-burn analyses card)
     (double (opponent-ruff-risk analyses card))))

(defn partner-control-burn-penalty [play-config game analyses card]
  (* (:partner-control-burn-penalty play-config 0)
     (if (= (:trump game) (rules/effective-suit card (:trump game)))
       (partner-control-burn analyses card)
       (ruff-exposed-control-burn analyses card))))

(defn preservation-lead-value [play-config game context analyses card]
  (let [{:keys [score risk] :as features} (lead-card-features game analyses card)]
    (- score
       (* (:lead-risk-penalty play-config) risk)
       (defender-preservation-penalty play-config context features)
       (partner-control-burn-penalty play-config game analyses card))))

(defn best-lead-by-value [value-fn cards]
  (first (sort-by value-fn > cards)))

(defn risk-adjusted-win-cost [play-config game analyses card]
  (+ (bot-cards/card-score game card)
     (* (:win-risk-penalty play-config) (bot-cards/card-risk analyses card))))

(defn safe-cards [play-config analyses threshold-key cards]
  (filter #(<= (bot-cards/card-risk analyses %) (threshold-key play-config)) cards))

(defn priority-lead-card [game player cards]
  (let [unseen-counts (bot-cards/unseen-card-counts game player)
        trump-control (when (bot-cards/contract-caller? game player)
                        (secure-trump-lead-card game unseen-counts cards))
        caller-pressure (when (bot-cards/contract-caller? game player)
                          (caller-pressure-lead-card game cards))]
    (or trump-control caller-pressure)))

(defn threshold-lead-card [play-config game player analyses cards]
  (let [priority (priority-lead-card game player cards)
        safe (safe-cards play-config analyses :lead-risk-tolerance cards)]
    (cond
      priority
      priority

      (seq safe)
      (bot-cards/lowest-card game safe)

      :else
      (bot-cards/lowest-card game cards))))

(defn risk-adjusted-lead-candidates [game player cards]
  (let [trump (:trump game)
        off-aces (seq (filter #(bot-cards/off-ace? trump %) cards))]
    (cond
      (bot-cards/contract-caller? game player)
      (or (seq (remove #(bot-cards/trump-card? trump %) cards))
          cards)

      off-aces
      off-aces

      :else
      cards)))

(defn defender-exit-lead-candidates [game player cards]
  (let [trump (:trump game)
        off-aces (seq (filter #(bot-cards/off-ace? trump %) cards))
        non-trumps (seq (remove #(bot-cards/trump-card? trump %) cards))]
    (cond
      (bot-cards/contract-caller? game player)
      (or non-trumps cards)

      off-aces
      off-aces

      non-trumps
      non-trumps

      :else
      cards)))

(defn defender-low-exit-card [game player cards]
  (let [{:keys [defender?]} (bot-cards/lead-context game player)
        trump (:trump game)
        off-aces (seq (filter #(bot-cards/off-ace? trump %) cards))
        non-trumps (seq (remove #(bot-cards/trump-card? trump %) cards))]
    (when (and defender?
               non-trumps
               (not off-aces))
      (bot-cards/lowest-card game non-trumps))))

(declare lower-preservation-winners)

(defn preservation-trump-lead-card [play-config game analyses cards]
  (when (and (seq cards)
             (every? #(bot-cards/trump-card? (:trump game) %) cards))
    (when-let [lower-winners (seq (lower-preservation-winners play-config
                                                              game
                                                              analyses
                                                              cards))]
      (bot-cards/lowest-card game lower-winners))))

(defn probability-lead-card-with-candidates
  [candidate-fn play-config game player analyses cards]
  (let [priority (priority-lead-card game player cards)
        safe (safe-cards play-config analyses :lead-risk-tolerance cards)
        fallback-cards (candidate-fn game player cards)]
    (cond
      priority
      priority

      (seq safe)
      (bot-cards/lowest-card game safe)

      :else
      (best-lead-by-value #(risk-adjusted-lead-value play-config
                                                    game
                                                    analyses
                                                    %)
                          fallback-cards))))

(defn probability-lead-card [play-config game player analyses cards]
  (probability-lead-card-with-candidates risk-adjusted-lead-candidates
                                         play-config
                                         game
                                         player
                                         analyses
                                         cards))

(defn defender-exit-probability-lead-card [play-config game player analyses cards]
  (let [priority (priority-lead-card game player cards)
        safe (safe-cards play-config analyses :lead-risk-tolerance cards)
        defender-low-exit (defender-low-exit-card game player cards)
        fallback-cards (defender-exit-lead-candidates game player cards)]
    (cond
      priority
      priority

      (seq safe)
      (bot-cards/lowest-card game safe)

      defender-low-exit
      defender-low-exit

      :else
      (best-lead-by-value #(risk-adjusted-lead-value play-config
                                                    game
                                                    analyses
                                                    %)
                          fallback-cards))))

(defn preservation-probability-lead-card [play-config game player analyses cards]
  (let [priority (when-let [priority-card (priority-lead-card game player cards)]
                   (when (zero? (partner-control-burn-penalty play-config
                                                               game
                                                               analyses
                                                               priority-card))
                     priority-card))
        safe (safe-cards play-config analyses :lead-risk-tolerance cards)
        defender-low-exit (defender-low-exit-card game player cards)
        trump-lead (preservation-trump-lead-card play-config game analyses cards)
        fallback-cards (risk-adjusted-lead-candidates game player cards)
        context (bot-cards/lead-context game player)]
    (cond
      priority
      priority

      (seq safe)
      (bot-cards/lowest-card game safe)

      defender-low-exit
      defender-low-exit

      trump-lead
      trump-lead

      :else
      (best-lead-by-value #(preservation-lead-value play-config
                                                    game
                                                    context
                                                    analyses
                                                    %)
                          fallback-cards))))

(defn ruff-invite-preservation-lead-card [play-config game player analyses cards]
  (or (partner-ruff-invite-card play-config
                                game
                                player
                                (bot-cards/unseen-card-counts game player)
                                cards)
      (preservation-probability-lead-card play-config game player analyses cards)))

;; ---------------------------------------------------------------------------
;; Team-EV lead evaluation
;; ---------------------------------------------------------------------------

(defn backup-secure-trump-after-spend? [game player unseen-counts card]
  (and (bot-cards/trump-card? (:trump game) card)
       (some #(and (bot-cards/trump-card? (:trump game) %)
                   (bot-cards/good-card-with-counts? game unseen-counts %))
             (bot-cards/remaining-hand-after game player card))))

(defn high-trump-spend-penalty
  ([play-config game card]
   (if (bot-cards/high-preservation-trump? game card)
     (:team-ev-high-trump-spend-penalty play-config)
     0))
  ([play-config game player unseen-counts card]
   (let [penalty (high-trump-spend-penalty play-config game card)]
     (if (and (pos? penalty)
              (backup-secure-trump-after-spend? game
                                                player
                                                unseen-counts
                                                card))
       (* penalty
          (- 1.0
             (:team-ev-backup-secure-trump-spend-discount play-config 0.0)))
       penalty))))

(defn card-spend-cost
  ([play-config game card]
   (+ (* (:team-ev-card-spend-rate play-config) (bot-cards/card-score game card))
      (high-trump-spend-penalty play-config game card)))
  ([play-config game player unseen-counts card]
   (+ (* (:team-ev-card-spend-rate play-config) (bot-cards/card-score game card))
      (high-trump-spend-penalty play-config game player unseen-counts card))))

(defn off-suit-control-card? [game unseen-counts card]
  (and (not (bot-cards/trump-card? (:trump game) card))
       (bot-cards/in-suit-control-card? game unseen-counts card)))

(defn future-off-suit-ruff-exposure [play-config game player unseen-counts card]
  (reduce max
          0.0
          (map (fn [future-card]
                 (let [lead (bot-cards/card-effective-suit game future-card)]
                   (* (/ (double (bot-cards/potential-card-score game future-card)) 80.0)
                      (inference/opponent-ruff-probability-for-lead play-config
                                                                    game
                                                                    player
                                                                    unseen-counts
                                                                    lead))))
               (filter #(off-suit-control-card? game unseen-counts %)
                       (bot-cards/remaining-hand-after game player card)))))

(defn secure-trump-protection-bonus [play-config game player unseen-counts card]
  (if (and (bot-cards/maker-team? game player)
           (bot-cards/trump-card? (:trump game) card)
           (bot-cards/good-card-with-counts? game unseen-counts card)
           (not (inference/opponents-likely-void-in-suit? play-config
                                                          game
                                                          player
                                                          unseen-counts
                                                          (inference/known-voids game)
                                                          player
                                                          (:trump game))))
    (* (:team-ev-secure-trump-protection-weight play-config 0)
       (future-off-suit-ruff-exposure play-config game player unseen-counts card))
    0.0))

(defn team-ev-lead-breakdown [play-config game player analyses unseen-counts card]
  (let [lead (rules/effective-suit card (:trump game))
        risk (bot-cards/card-risk analyses card)
        partner-ruff (inference/partner-ruff-probability-for-lead play-config
                                                                  game
                                                                  player
                                                                  unseen-counts
                                                                  lead)
        opponent-ruff (inference/opponent-ruff-probability-for-lead play-config
                                                                    game
                                                                    player
                                                                    unseen-counts
                                                                    lead)
        team-win-prob (min 1.0 (inference/combined-probability [(- 1.0 risk)
                                                                partner-ruff]))
        safe? (zero? risk)
        protection-bonus (secure-trump-protection-bonus play-config
                                                        game
                                                        player
                                                        unseen-counts
                                                        card)
        spend-cost (card-spend-cost play-config game player unseen-counts card)
        partner-control-burn (partner-control-burn analyses card)
        ruff-exposed-burn (ruff-exposed-control-burn analyses card)
        control-burn-penalty (partner-control-burn-penalty play-config game analyses card)
        value (- (+ (* (:team-ev-trick-weight play-config) team-win-prob)
                    (* (:team-ev-partner-ruff-weight play-config) partner-ruff)
                    (if safe? (:team-ev-safe-card-bonus play-config) 0)
                    protection-bonus)
                 (* (:team-ev-risk-penalty play-config) risk)
                 (* (:team-ev-opponent-ruff-penalty play-config) opponent-ruff)
                 control-burn-penalty
                 spend-cost)]
    {:card card
     :lead lead
     :risk risk
     :team-win-prob team-win-prob
     :partner-ruff-prob partner-ruff
     :opponent-ruff-prob opponent-ruff
     :partner-control-burn partner-control-burn
     :ruff-exposed-control-burn ruff-exposed-burn
     :control-burn-penalty control-burn-penalty
     :protection-bonus protection-bonus
     :spend-cost spend-cost
     :value value}))

(defn team-ev-lead-value [play-config game player analyses unseen-counts card]
  (:value (team-ev-lead-breakdown play-config game player analyses unseen-counts card)))

(defn dead-lead-exit-card
  "When every lead is very likely to lose, preserve higher cards instead of
  spending them for tiny local risk differences."
  [play-config game analyses cards]
  (let [threshold (:team-ev-dead-lead-risk-threshold play-config 1.0)]
    (when (and (seq cards)
               (every? #(>= (bot-cards/card-risk analyses %) threshold) cards))
      (bot-cards/lowest-card game cards))))

(defn team-ev-probability-lead-card [play-config game player analyses cards]
  (or (dead-lead-exit-card play-config game analyses cards)
      (let [unseen-counts (bot-cards/unseen-card-counts game player)]
        (best-lead-by-value #(team-ev-lead-value play-config
                                                 game
                                                 player
                                                 analyses
                                                 unseen-counts
                                                 %)
                            cards))))

(defn karbosh-caller-lead-card [play-config game player analyses cards]
  (let [trumps (filter #(bot-cards/trump-card? (:trump game) %) cards)]
    (if (seq trumps)
      (first (sort-by #(risk-adjusted-lead-value play-config game analyses %)
                      >
                      trumps))
      (probability-lead-card play-config game player analyses cards))))

;; ---------------------------------------------------------------------------
;; Winning-card selection
;; ---------------------------------------------------------------------------

(defn probability-winning-card [play-config game _player analyses _cards winning-cards]
  (let [safe (safe-cards play-config analyses :win-risk-tolerance winning-cards)]
    (if (seq safe)
      (bot-cards/lowest-card game safe)
      (first (sort-by #(risk-adjusted-win-cost play-config game analyses %)
                      winning-cards)))))

(defn unsafe-high-trump-winner? [play-config game analyses card]
  (and (bot-cards/high-preservation-trump? game card)
       (> (bot-cards/card-risk analyses card)
          (:win-risk-tolerance play-config))))

(defn lower-preservation-winners [play-config game analyses winning-cards]
  (let [unsafe-highs (filter #(unsafe-high-trump-winner?
                                play-config
                                game
                                analyses
                                %)
                             winning-cards)]
    (when (seq unsafe-highs)
      (seq (remove #(some #{%} unsafe-highs) winning-cards)))))

(defn preservation-winning-card
  [play-config game player analyses cards winning-cards]
  (let [safe (safe-cards play-config analyses :win-risk-tolerance winning-cards)
        lower-winners (lower-preservation-winners play-config
                                                  game
                                                  analyses
                                                  winning-cards)
        non-winning (seq (remove #(bot-cards/wins-trick? game player %) cards))]
    (cond
      (seq safe)
      (bot-cards/lowest-card game safe)

      lower-winners
      (bot-cards/lowest-card game lower-winners)

      (and non-winning
           (seq (bot-cards/pending-partners-after game player))
           (every? #(unsafe-high-trump-winner? play-config game analyses %)
                   winning-cards))
      (bot-cards/lowest-card game non-winning)

      :else
      (first (sort-by #(risk-adjusted-win-cost play-config game analyses %)
                      winning-cards)))))

;; ---------------------------------------------------------------------------
;; Probability engines
;; ---------------------------------------------------------------------------

(defn probability-card-action-with-lead
  ([play-config lead-card-fn game player]
   (probability-card-action-with-lead play-config
                                      lead-card-fn
                                      probability-winning-card
                                      game
                                      player))
  ([play-config lead-card-fn winning-card-fn game player]
   (let [cards (vec (bot-cards/legal-cards game player))
         winner (bot-cards/current-trick-winner game)
         config (context-play-config play-config game player)
         unseen-counts (bot-cards/unseen-card-counts game player)
         analyses (bot-cards/card-analyses game player cards)
         winning-cards (filter #(bot-cards/wins-trick? game player %) cards)
         card (cond
                (empty? cards)
                nil

                (empty? (:current-trick game))
                (if (bot-cards/special-contract-caller? game player)
                  (karbosh-caller-lead-card config game player analyses cards)
                  (lead-card-fn config game player analyses cards))

                (bot-cards/same-team? game player winner)
                (partner-preserving-card config game player unseen-counts cards)

                (seq winning-cards)
                (winning-card-fn config game player analyses cards winning-cards)

                :else
                (ditch-card config game player unseen-counts cards))]
     (when card
       {:type :play-card
        :card card}))))

(defn threshold-probability-card-action [play-config game player]
  (probability-card-action-with-lead play-config threshold-lead-card game player))

(defn probability-card-action [play-config game player]
  (probability-card-action-with-lead play-config probability-lead-card game player))

(defn defender-exit-probability-card-action [play-config game player]
  (probability-card-action-with-lead play-config
                                     defender-exit-probability-lead-card
                                     game
                                     player))

(defn preservation-probability-card-action [play-config game player]
  (probability-card-action-with-lead play-config
                                     preservation-probability-lead-card
                                     preservation-winning-card
                                     game
                                     player))

(defn ruff-invite-preservation-card-action [play-config game player]
  (probability-card-action-with-lead play-config
                                     ruff-invite-preservation-lead-card
                                     preservation-winning-card
                                     game
                                     player))

(defn team-ev-probability-card-action [play-config game player]
  (probability-card-action-with-lead play-config
                                     team-ev-probability-lead-card
                                     preservation-winning-card
                                     game
                                     player))
