(ns clojure-card-games.karbosh.bot.config
  "Bot policy configuration.

  All bid and play weights live in `policy.edn` next to the karbosh sources
  so tuning changes are explicit, diffable data changes rather than edits
  buried in policy code. The file is validated here at load time: every
  required key must be present, no unknown keys are allowed, and values must
  have the expected shape."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def default-ditch-policy :future-suit-equity)
(def classic-ditch-policy :classic)
(def ditch-policies #{default-ditch-policy classic-ditch-policy})

(def bid-config-keys
  #{:bid-4-strength
    :bid-5-strength
    :bid-6-strength
    :high-trump-strength
    :karbosh-min-trumps
    :karbosh-min-high-trumps
    :karbosh-min-bowers
    :karbosh-min-winners
    :karbosh-target-prob
    :karbosh-desperate-target-prob
    :karbosh-protect-target-prob
    :karbosh-score-context-band
    :karbosh-failure-opponent-tricks
    :karbosh-prob-intercept
    :karbosh-prob-trump-weight
    :karbosh-prob-high-trump-weight
    :karbosh-prob-bower-weight
    :karbosh-prob-right-bower-weight
    :karbosh-prob-left-bower-weight
    :karbosh-prob-off-ace-weight
    :karbosh-prob-low-trump-weight
    :karbosh-prob-off-junk-weight
    :karbosh-prob-missing-bower-weight
    :karbosh-prob-donation-help-weight
    :karbosh-prob-donation-pair-weight
    :karbosh-donation-qualification-prob
    :karbosh-donor-hand-size
    :double-karbosh-score-context-band
    :conservative-bid-min-controls})

(def play-config-keys
  #{:lead-risk-tolerance
    :win-risk-tolerance
    :lead-risk-penalty
    :win-risk-penalty
    :defender-high-trump-preservation-penalty
    :team-ev-trick-weight
    :team-ev-partner-ruff-weight
    :team-ev-opponent-ruff-penalty
    :team-ev-risk-penalty
    :team-ev-high-trump-spend-penalty
    :team-ev-card-spend-rate
    :team-ev-safe-card-bonus
    :team-ev-backup-secure-trump-spend-discount
    :team-ev-secure-trump-protection-weight
    :ditch-policy
    :ditch-future-suit-equity-weight
    :soft-void-trump-threshold
    :action-inference-confidence
    :karbosh-lead-risk-tolerance
    :karbosh-win-risk-tolerance
    :karbosh-lead-risk-penalty
    :karbosh-win-risk-penalty})

(defn- valid-config-value? [k v]
  (case k
    :ditch-policy (contains? ditch-policies v)
    :conservative-bid-min-controls (and (map? v)
                                        (every? number? (keys v))
                                        (every? number? (vals v)))
    (number? v)))

(defn validate-config!
  "Throw when `config` does not contain exactly `required` well-typed keys."
  [label required config]
  (let [present (set (keys config))
        missing (set/difference required present)
        unknown (set/difference present required)
        bad-values (into {}
                         (remove (fn [[k v]] (valid-config-value? k v)))
                         config)]
    (when (or (seq missing) (seq unknown) (seq bad-values))
      (throw (ex-info (str "Invalid bot policy config: " label)
                      {:config label
                       :missing-keys missing
                       :unknown-keys unknown
                       :bad-values bad-values})))
    config))

(def ^:private policy-resource "clojure_card_games/karbosh/policy.edn")

(defn load-policy
  "Read and validate the policy EDN from the classpath."
  []
  (let [resource (or (io/resource policy-resource)
                     (throw (ex-info "Bot policy file not found on classpath"
                                     {:resource policy-resource})))
        policy (edn/read-string (slurp resource))
        classic (get-in policy [:play :classic])]
    (validate-config! :bid-default bid-config-keys (get-in policy [:bid :default]))
    (validate-config! :play-classic play-config-keys classic)
    (validate-config! :play-default play-config-keys
                      (merge classic (get-in policy [:play :default-overrides])))
    (validate-config! :play-action-inference play-config-keys
                      (merge classic
                             (get-in policy [:play :default-overrides])
                             (get-in policy [:play :action-inference-overrides])))
    policy))

(def policy (load-policy))

(def default-bid-config (get-in policy [:bid :default]))

(def classic-play-config (get-in policy [:play :classic]))

(def default-play-config
  (merge classic-play-config (get-in policy [:play :default-overrides])))

(def action-inference-play-config
  (merge default-play-config (get-in policy [:play :action-inference-overrides])))

(def default-bid-strategy (get-in policy [:strategies :bid]))
(def default-play-strategy (get-in policy [:strategies :play]))
