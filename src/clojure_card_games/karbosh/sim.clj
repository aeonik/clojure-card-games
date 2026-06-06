(ns clojure-card-games.karbosh.sim
  (:require [clojure.pprint :as pprint]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.game :as game]))

(def default-options
  {:max-hands 100
   :min-score -100
   :max-events-per-hand 256
   :collect-analysis? false
   :bid-strategy bot/default-bid-strategy
   :play-strategy bot/default-play-strategy
   :play-strategy-by-team nil
   :play-strategy-by-player nil
   :play-config bot/default-play-config
   :play-config-by-team nil
   :play-config-by-player nil
   :bid-config bot/default-bid-config})

(defn min-score-reached? [state min-score]
  (some #(<= % min-score) (vals (:scores state))))

(defn stop-reason [{:keys [max-hands min-score]} state]
  (cond
    (= :game-over (:phase state))
    :target-score

    (>= (count (:hand-history state)) max-hands)
    :max-hands

    (min-score-reached? state min-score)
    :min-score

    :else
    nil))

(defn playable-phase? [state]
  (contains? #{:bidding
               :trump-selection
               :karbosh-discard
               :karbosh-donation
               :trick-playing}
             (:phase state)))

(defn play-strategy-for [options state player]
  (or (get-in options [:play-strategy-by-player player])
      (get-in options [:play-strategy-by-team (game/player-team state player)])
      (:play-strategy options)))

(defn play-config-for [options state player]
  (or (get-in options [:play-config-by-player player])
      (get-in options [:play-config-by-team (game/player-team state player)])
      (:play-config options)))

(defn bot-event [state options]
  (when (playable-phase? state)
    (when-let [player (:current-player state)]
      (binding [bot/*play-strategy* (play-strategy-for options state player)
                bot/*play-config* (play-config-for options state player)]
        (bot/action state player)))))

(defn advance-event [state options]
  (case (:phase state)
    :hand-complete {:type :new-hand}
    (bot-event state options)))

(defn playable-event [state event]
  (if (:player event)
    event
    (cond-> event
      (playable-phase? state)
      (assoc :player (:current-player state)))))

(defn analysis-snapshot [state event]
  (when (and (playable-phase? state)
             (:player event))
    {:hand-index (:hand-index state)
     :phase (:phase state)
     :player (:player event)
     :event event
     :analysis (bot/hypergeom-analysis state (:player event))}))

(defn advance
  ([state] (advance state default-options))
  ([state options]
   (if-let [event (advance-event state options)]
     (let [event (playable-event state event)
           snapshot (when (:collect-analysis? options)
                      (analysis-snapshot state event))]
       (cond-> (game/apply-event state event)
         snapshot
         (update :sim/analysis (fnil conj []) snapshot)))
     state)))

(defn run-hand [state {:keys [max-events-per-hand] :as options}]
  (loop [state state
         events 0]
    (cond
      (or (not (playable-phase? state))
          (stop-reason options state))
      state

      (>= events max-events-per-hand)
      (assoc state :sim/error :max-events-per-hand)

      :else
      (recur (advance state options) (inc events)))))

(defn run-game
  ([seed] (run-game seed default-options))
  ([seed options]
   (let [options (merge default-options options)]
     (binding [bot/*bid-config* (:bid-config options)
               bot/*bid-strategy* (:bid-strategy options)
               bot/*play-strategy* (:play-strategy options)
               bot/*play-config* (:play-config options)]
       (loop [state (game/init-game seed)]
         (let [state (run-hand state options)
               reason (or (:sim/error state)
                          (stop-reason options state))]
           (if reason
             (assoc state :sim/stop-reason reason)
             (recur (advance state options)))))))))

(defn bid-key [{:keys [bid-type value]}]
  (if bid-type
    (case bid-type
      :bid value
      bid-type)
    :all-pass))

(defn bid-target [{:keys [bid-type value]}]
  (case bid-type
    :bid value
    (:karbosh :double-karbosh) 8
    nil))

(defn bid-outcome [{:keys [bid tricks points] :as summary}]
  (let [team (some->> bid :player (get (game/teams)))
        target (some-> bid bid-target)
        taken (when team (get tricks team 0))]
    (cond-> {:hand-index (:hand-index summary)
             :bid-key (bid-key bid)
             :bid bid
             :tricks tricks
             :points points}
      team (assoc :team team)
      target (assoc :target target)
      taken (assoc :taken taken)
      (and target taken)
      (assoc :made? (>= taken target)
             :margin (- taken target)))))

(defn summarize-outcomes [outcomes]
  (into {}
        (for [[bid-key outcomes] (group-by :bid-key outcomes)]
          (let [attempts (count outcomes)
                decided (filter :target outcomes)
                made (count (filter :made? decided))
                margins (keep :margin decided)]
            [bid-key
             (cond-> {:attempts attempts}
               (seq decided)
               (assoc :made made
                      :failed (- (count decided) made)
                      :make-rate (double (/ made (count decided))))
               (seq margins)
               (assoc :avg-margin (double (/ (reduce + margins)
                                              (count margins)))))]))))

(defn summarize-game [state]
  (let [hands (:hand-history state)
        outcomes (mapv bid-outcome hands)
        bid-frequencies (frequencies (map :bid-key outcomes))
        stop-reason (:sim/stop-reason state)]
    {:stop-reason stop-reason
     :hands (count hands)
     :scores (:scores state)
     :winner (:winner state)
     :bid-frequencies bid-frequencies
     :bid-outcomes outcomes
     :bid-results (summarize-outcomes outcomes)
     :karbosh-attempts (get bid-frequencies :karbosh 0)
     :double-karbosh-attempts (get bid-frequencies :double-karbosh 0)
     :analysis-snapshots (count (:sim/analysis state))}))

(defn run-games-for-seeds
  ([seeds] (run-games-for-seeds seeds default-options))
  ([seeds options]
   (mapv (fn [seed]
           (let [state (run-game seed options)]
             (assoc (summarize-game state) :seed seed)))
         seeds)))

(defn run-games
  ([n] (run-games n default-options))
  ([n options]
   (run-games-for-seeds (range n) options)))

(defn aggregate [results]
  (let [outcomes (mapcat :bid-outcomes results)
        games (count results)
        winners (frequencies (map #(or (:winner %) :none) results))]
    {:games (count results)
     :hands (reduce + (map :hands results))
     :stop-reasons (frequencies (map :stop-reason results))
     :winners winners
     :win-rates (into {}
                      (map (fn [[team wins]]
                             [team (if (pos? games)
                                     (double (/ wins games))
                                     0.0)]))
                      winners)
     :bid-frequencies (frequencies (map :bid-key outcomes))
     :bid-results (summarize-outcomes outcomes)
     :karbosh-attempts (count (filter #(= :karbosh (:bid-key %)) outcomes))
     :double-karbosh-attempts (count (filter #(= :double-karbosh (:bid-key %)) outcomes))}))

(defn policy-winner [policy-by-team result]
  (some->> (:winner result)
           (get policy-by-team)))

(defn aggregate-policy-wins [policy-results]
  (let [games (count policy-results)
        winners (frequencies (map #(or (:policy-winner %) :none) policy-results))]
    {:games games
     :winners winners
     :win-rates (into {}
                      (map (fn [[policy wins]]
                             [policy (if (pos? games)
                                       (double (/ wins games))
                                       0.0)]))
                      winners)}))

(defn play-strategy-matchup
  "Compare two play strategies head-to-head on the same seeds, swapping teams to
  reduce seat bias."
  ([left right seeds] (play-strategy-matchup left right seeds default-options))
  ([[left-label left-strategy] [right-label right-strategy] seeds options]
   (let [forward-policies {1 left-label 2 right-label}
         reverse-policies {1 right-label 2 left-label}
         forward-options (assoc options
                           :play-strategy-by-team {1 left-strategy
                                                   2 right-strategy})
         reverse-options (assoc options
                           :play-strategy-by-team {1 right-strategy
                                                   2 left-strategy})
         attach-winner (fn [policy-by-team result]
                         (assoc result
                                :policy-by-team policy-by-team
                                :policy-winner (policy-winner policy-by-team
                                                              result)))
         forward (mapv #(attach-winner forward-policies %)
                       (run-games-for-seeds seeds forward-options))
         reverse (mapv #(attach-winner reverse-policies %)
                       (run-games-for-seeds seeds reverse-options))
         results (vec (concat forward reverse))]
     (assoc (aggregate-policy-wins results)
            :labels [left-label right-label]
            :seeds (count seeds)
            :forward (aggregate forward)
            :reverse (aggregate reverse)))))

(defn evaluate-bid-configs
  "Run named bid configs against the same seeds so tuning comparisons are
  driven by policy differences, not different shuffled hands."
  ([configs seeds] (evaluate-bid-configs configs seeds default-options))
  ([configs seeds options]
   (mapv (fn [[label bid-config]]
           (assoc (aggregate
                    (run-games-for-seeds seeds
                                         (assoc options :bid-config bid-config)))
                  :label label
                  :bid-config bid-config))
         configs)))

(defn evaluate-play-strategies
  "Run named play strategies against the same seeds so card-play changes can be
  compared without changing the deal set."
  ([strategies seeds] (evaluate-play-strategies strategies seeds default-options))
  ([strategies seeds options]
   (mapv (fn [[label strategy]]
           (assoc (aggregate
                    (run-games-for-seeds seeds
                                         (assoc options :play-strategy strategy)))
                  :label label
                  :play-strategy strategy))
         strategies)))

(defn evaluate-bid-strategies
  "Run named bid strategies against the same seeds so bidding changes can be
  compared without changing the deal set."
  ([strategies seeds] (evaluate-bid-strategies strategies seeds default-options))
  ([strategies seeds options]
   (mapv (fn [[label strategy]]
           (assoc (aggregate
                    (run-games-for-seeds seeds
                                         (assoc options :bid-strategy strategy)))
                  :label label
                  :bid-strategy strategy))
         strategies)))

(defn -main [& args]
  (let [n (if-let [arg (first args)]
            (Long/parseLong arg)
            100)]
    (pprint/pprint (aggregate (run-games n)))))
