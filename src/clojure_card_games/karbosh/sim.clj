(ns clojure-card-games.karbosh.sim
  (:require [clojure.pprint :as pprint]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.game :as game]))

(def default-options
  {:max-hands 100
   :min-score -100
   :max-events-per-hand 256
   :collect-analysis? false
   :play-strategy bot/default-play-strategy
   :play-config bot/default-play-config
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

(defn bot-event [state]
  (when (playable-phase? state)
    (some->> (:current-player state)
             (bot/action state))))

(defn advance-event [state]
  (case (:phase state)
    :hand-complete {:type :new-hand}
    (bot-event state)))

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
   (if-let [event (advance-event state)]
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
  (let [outcomes (mapcat :bid-outcomes results)]
    {:games (count results)
     :hands (reduce + (map :hands results))
     :stop-reasons (frequencies (map :stop-reason results))
     :bid-frequencies (frequencies (map :bid-key outcomes))
     :bid-results (summarize-outcomes outcomes)
     :karbosh-attempts (count (filter #(= :karbosh (:bid-key %)) outcomes))
     :double-karbosh-attempts (count (filter #(= :double-karbosh (:bid-key %)) outcomes))}))

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

(defn -main [& args]
  (let [n (if-let [arg (first args)]
            (Long/parseLong arg)
            100)]
    (pprint/pprint (aggregate (run-games n)))))
