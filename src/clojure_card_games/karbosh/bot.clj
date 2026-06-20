(ns clojure-card-games.karbosh.bot
  "Facade for the Karbosh bot.

  The implementation lives in focused submodules that all take their policy
  config explicitly:

  - `bot.config`    policy weights loaded and validated from `policy.edn`
  - `bot.cards`     card scoring, hand bookkeeping, analysis delegation
  - `bot.inference` known voids, soft/action inference, ruff probabilities
  - `bot.bid`       bidding features and contract evaluation
  - `bot.play`      lead/winning-card selection, ditching, team-EV
  - `bot.explain`   structured decision explanations

  This namespace owns the dynamic strategy/config selection (`*bid-config*`,
  `*play-config*`, `*bid-strategy*`, `*play-strategy*`), the strategy
  registries, and the public entry points that the server, room, sim, and
  workbench bind and call."
  (:require [clojure-card-games.karbosh.bot.bid :as bid]
            [clojure-card-games.karbosh.bot.cards :as bot-cards]
            [clojure-card-games.karbosh.bot.config :as config]
            [clojure-card-games.karbosh.bot.explain :as explain]
            [clojure-card-games.karbosh.bot.inference :as inference]
            [clojure-card-games.karbosh.bot.play :as play]
            [clojure-card-games.karbosh.shared.game :as game]))

;; ---------------------------------------------------------------------------
;; Policy configuration and dynamic selection
;; ---------------------------------------------------------------------------

(def default-bid-config config/default-bid-config)
(def default-bid-strategy config/default-bid-strategy)

(def default-ditch-policy config/default-ditch-policy)
(def classic-ditch-policy config/classic-ditch-policy)
(def ditch-policies config/ditch-policies)

(def classic-play-config config/classic-play-config)
(def default-play-config config/default-play-config)
(def action-inference-play-config config/action-inference-play-config)
(def default-play-strategy config/default-play-strategy)

(def ^:dynamic *bid-config* default-bid-config)
(def ^:dynamic *bid-strategy* default-bid-strategy)
(def ^:dynamic *play-config* default-play-config)
(def ^:dynamic *play-strategy* default-play-strategy)

;; ---------------------------------------------------------------------------
;; Re-exported helpers
;; ---------------------------------------------------------------------------

(def suit-strength bid/suit-strength)
(def best-trump bid/best-trump)

(def strongest-card-for-trump bot-cards/strongest-card-for-trump)
(def karbosh-discard-cards bot-cards/karbosh-discard-cards)
(def legal-cards bot-cards/legal-cards)
(def completed-trick-cards bot-cards/completed-trick-cards)
(def current-trick-cards bot-cards/current-trick-cards)
(def public-played-cards bot-cards/public-played-cards)
(def seen-cards bot-cards/seen-cards)
(def unseen-cards bot-cards/unseen-cards)
(def unseen-card-counts bot-cards/unseen-card-counts)
(def hypergeom-analysis bot-cards/hypergeom-analysis)
(def card-analyses bot-cards/card-analyses)
(def card-score bot-cards/card-score)
(def good-card? bot-cards/good-card?)
(def secure-winning-card? bot-cards/secure-winning-card?)
(def round-probability bot-cards/round-probability)
(def special-contract? bot-cards/special-contract?)

(def known-voids inference/known-voids)
(def total-unseen-count inference/total-unseen-count)
(def unseen-effective-suit-count inference/unseen-effective-suit-count)
(def prob-hand-has-success inference/prob-hand-has-success)
(def soft-void-confidence inference/soft-void-confidence)
(def suit-available-probability inference/suit-available-probability)

(def karbosh-donation-analysis bid/karbosh-donation-analysis)
(def karbosh-hand? bid/karbosh-hand?)
(def double-karbosh-evaluation bid/double-karbosh-evaluation)
(def karbosh-evaluation bid/karbosh-evaluation)
(def karbosh-target-prob bid/karbosh-target-prob)

(def ditch-card play/ditch-card)
(def team-ev-lead-breakdown play/team-ev-lead-breakdown)

(def candidate-summary explain/candidate-summary)
(def strategy-key explain/strategy-key)
(def hybrid-engines explain/hybrid-engines)
(def card-engine explain/card-engine)

;; ---------------------------------------------------------------------------
;; Bidding entry points
;; ---------------------------------------------------------------------------

(defn target-bid
  ([hand] (target-bid *bid-config* hand))
  ([config hand] (bid/target-bid config hand)))

(defn threshold-bid-action [game player]
  (bid/threshold-bid-action *bid-config* game player))

(defn probability-bid-action [game player]
  (bid/probability-bid-action *bid-config* game player))

(defn conservative-probability-bid-action [game player]
  (bid/conservative-probability-bid-action *bid-config* game player))

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

(defn donate-action [game player]
  (when-let [card (bot-cards/highest-card game (get-in game [:players player :hand]))]
    {:type :donate-card
     :card card}))

(defn discard-action [game player]
  (when-let [card (bot-cards/lowest-card game (get-in game [:players player :hand]))]
    {:type :discard-card
     :card card}))

;; ---------------------------------------------------------------------------
;; Play strategies
;; ---------------------------------------------------------------------------

(defn card-counting-card-action [game player]
  (play/card-counting-card-action *play-config* game player))

(defn threshold-probability-card-action [game player]
  (play/threshold-probability-card-action *play-config* game player))

(defn probability-card-action [game player]
  (play/probability-card-action *play-config* game player))

(defn defender-exit-probability-card-action [game player]
  (play/defender-exit-probability-card-action *play-config* game player))

(defn preservation-probability-card-action [game player]
  (play/preservation-probability-card-action *play-config* game player))

(defn ruff-invite-preservation-card-action [game player]
  (play/ruff-invite-preservation-card-action *play-config* game player))

(defn team-ev-probability-card-action [game player]
  (play/team-ev-probability-card-action *play-config* game player))

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

(defn with-action-inference [f game player]
  (binding [*play-config* (assoc *play-config*
                                 :action-inference-confidence
                                 (:action-inference-confidence
                                  action-inference-play-config))]
    (f game player)))

(defn action-inference-team-ev-probability-card-action [game player]
  (with-action-inference team-ev-probability-card-action game player))

(defn action-inference-preservation-probability-card-action [game player]
  (with-action-inference preservation-probability-card-action game player))

(defn action-inference-ruff-invite-probability-card-action [game player]
  (with-action-inference ruff-invite-preservation-card-action game player))

(defn action-inference-preservation-hybrid-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (action-inference-preservation-probability-card-action game player)))

(defn action-inference-ruff-invite-hybrid-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (action-inference-ruff-invite-probability-card-action game player)))

(defn action-inference-team-ev-hybrid-card-action [game player]
  (with-action-inference team-ev-hybrid-card-action game player))

(def play-strategies
  {:card-counting card-counting-card-action
   :probability-threshold threshold-probability-card-action
   :probability probability-card-action
   :probability-defender-exit defender-exit-probability-card-action
   :probability-preservation preservation-probability-card-action
   :probability-ruff-invite ruff-invite-preservation-card-action
   :probability-action-inference-preservation action-inference-preservation-probability-card-action
   :probability-action-inference-ruff-invite action-inference-ruff-invite-probability-card-action
   :probability-team-ev team-ev-probability-card-action
   :probability-action-inference-team-ev action-inference-team-ev-probability-card-action
   :hybrid-threshold hybrid-threshold-card-action
   :hybrid hybrid-card-action
   :hybrid-defender-exit defender-exit-hybrid-card-action
   :hybrid-preservation preservation-hybrid-card-action
   :hybrid-ruff-invite ruff-invite-hybrid-card-action
   :hybrid-action-inference-preservation action-inference-preservation-hybrid-card-action
   :hybrid-action-inference-ruff-invite action-inference-ruff-invite-hybrid-card-action
   :hybrid-team-ev team-ev-hybrid-card-action
   :hybrid-action-inference-team-ev action-inference-team-ev-hybrid-card-action})

(defn resolve-play-strategy [strategy]
  (cond
    (fn? strategy) strategy
    (keyword? strategy) (or (get play-strategies strategy)
                            (throw (ex-info "Unknown play strategy"
                                            {:strategy strategy
                                             :available (keys play-strategies)})))
    :else (throw (ex-info "Invalid play strategy" {:strategy strategy}))))

;; ---------------------------------------------------------------------------
;; Explanations
;; ---------------------------------------------------------------------------

(defn explain-card-action [game player strategy event]
  (explain/explain-card-action *play-config* game player strategy event))

(def explain-bid-action explain/explain-bid-action)
(def explain-trump-action explain/explain-trump-action)
(def explain-donation-action explain/explain-donation-action)
(def explain-discard-action explain/explain-discard-action)

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

;; ---------------------------------------------------------------------------
;; Top-level actions
;; ---------------------------------------------------------------------------

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
