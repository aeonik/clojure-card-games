(ns clojure-card-games.karbosh.bot.explain
  "Structured explanations for bot decisions, consumed by the admin and
  workbench views and stored on AI events as `:ai` metadata."
  (:require [clojure-card-games.karbosh.bot.cards :as bot-cards]
            [clojure-card-games.karbosh.bot.bid :as bid]
            [clojure-card-games.karbosh.bot.config :as config]
            [clojure-card-games.karbosh.bot.play :as play]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]))

(defn strategy-key [strategy fallback]
  (if (keyword? strategy) strategy fallback))

(def hybrid-engines
  {:hybrid-threshold :probability-threshold
   :hybrid :probability
   :hybrid-defender-exit :probability-defender-exit
   :hybrid-preservation :probability-preservation
   :hybrid-ruff-invite :probability-ruff-invite
   :hybrid-action-inference-preservation :probability-action-inference-preservation
   :hybrid-action-inference-ruff-invite :probability-action-inference-ruff-invite
   :hybrid-team-ev :probability-team-ev
   :hybrid-action-inference-team-ev :probability-action-inference-team-ev})

(defn card-engine [game strategy]
  (let [strategy (strategy-key strategy :custom)]
    (if (and (contains? hybrid-engines strategy)
             (bot-cards/special-contract? (game/current-bid game)))
      :card-counting
      (get hybrid-engines strategy strategy))))

(defn candidate-summary [game player analyses card]
  (let [{:keys [higher-unseen
                higher-follow-unseen
                higher-trump-unseen
                prob-pending-opponent-has-higher-card
                prob-pending-opponent-has-higher-follow-card
                prob-pending-opponent-void-and-higher-trump
                prob-pending-opponent-can-beat-card
                prob-pending-partner-forced-higher-follow
                expected-pending-partner-control-burn]} (get analyses card)]
    {:card card
     :score (bot-cards/card-score game card)
     :risk (bot-cards/round-probability (bot-cards/card-risk analyses card))
     :good? (zero? (bot-cards/card-risk analyses card))
     :trump? (bot-cards/trump-card? (:trump game) card)
     :winning? (bot-cards/wins-trick? game player card)
     :hypergeom {:higher-unseen higher-unseen
                 :higher-follow-unseen higher-follow-unseen
                 :higher-trump-unseen higher-trump-unseen
                 :prob-any-higher (bot-cards/round-probability
                                   prob-pending-opponent-has-higher-card)
                 :prob-higher-follow (bot-cards/round-probability
                                      prob-pending-opponent-has-higher-follow-card)
                 :prob-void-higher-trump-by-player
                 (into {}
                       (map (fn [[player p]]
                              [player (bot-cards/round-probability p)]))
                       prob-pending-opponent-void-and-higher-trump)
                 :prob-can-beat (bot-cards/round-probability
                                 prob-pending-opponent-can-beat-card)
                 :prob-partner-forced-higher-follow-by-player
                 (into {}
                       (map (fn [[player p]]
                              [player (bot-cards/round-probability p)]))
                       prob-pending-partner-forced-higher-follow)
                 :expected-partner-control-burn
                 (bot-cards/round-probability
                  expected-pending-partner-control-burn)
                 :expected-partner-control-burn-exact
                 (some-> expected-pending-partner-control-burn str)}}))

(defn card-reason [play-config game player engine cards analyses card]
  (let [winner (bot-cards/current-trick-winner game)
        leading? (empty? (:current-trick game))
        partner-winning? (bot-cards/same-team? game player winner)
        selected-risk (bot-cards/card-risk analyses card)
        winning? (bot-cards/wins-trick? game player card)
        good? (zero? selected-risk)
        config (play/context-play-config play-config game player)
        unseen-counts (bot-cards/unseen-card-counts game player)
        winning-cards (filter #(bot-cards/wins-trick? game player %) cards)
        lower-winners (play/lower-preservation-winners config
                                                       game
                                                       analyses
                                                       winning-cards)
        ruff-invite (play/partner-ruff-invite-card config
                                                   game
                                                   player
                                                   unseen-counts
                                                   cards)
        defender-low-exit (play/defender-low-exit-card game player cards)
        trump-lead (play/preservation-trump-lead-card config game analyses cards)]
    (cond
      leading?
      (cond
        (= card ruff-invite)
        :partner-ruff-invite

        (= card defender-low-exit)
        :defender-low-exit

        (= card trump-lead)
        :lead-preserve-high-trump-winner

        (and (bot-cards/special-contract-caller? game player)
             (bot-cards/trump-card? (:trump game) card))
        :karbosh-caller-trump-control

        good?
        :lead-safe-card

        (#{:probability-preservation
           :probability-ruff-invite
           :probability-action-inference-preservation
           :probability-action-inference-ruff-invite
           :hybrid-preservation
           :hybrid-ruff-invite
           :hybrid-action-inference-preservation
           :hybrid-action-inference-ruff-invite} engine)
        :lead-preserve-high-trump

        (#{:probability-team-ev
           :probability-action-inference-team-ev} engine)
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
           (= card (bot-cards/lowest-card game lower-winners)))
      :preserve-high-trump-winner

      winning?
      :risk-adjusted-winning-card

      :else
      (if (= card (play/ditch-card config game player unseen-counts cards))
        :strategic-ditch
        :cannot-win-lowest-card))))

(defn explain-card-action [play-config game player strategy event]
  (let [strategy (strategy-key strategy :custom)
        engine (card-engine game strategy)
        cards (vec (bot-cards/legal-cards game player))
        analyses (bot-cards/card-analyses game player cards)
        card (:card event)]
    {:source :ai
     :phase :trick-playing
     :policy strategy
     :engine engine
     :ditch-policy (:ditch-policy play-config config/default-ditch-policy)
     :reason (card-reason play-config game player engine cards analyses card)
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
                               [suit (bid/suit-strength hand suit)]))
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
              :score (bot-cards/card-score game (:card event))}})

(defn explain-discard-action [game event]
  {:source :ai
   :phase :karbosh-discard
   :policy :discard-lowest-card
   :engine :card-strength
   :reason :discard-weakest-card
   :selected {:card (:card event)
              :score (bot-cards/card-score game (:card event))}})
