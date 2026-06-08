(ns clojure-card-games.karbosh.sim.personality
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.sim :as sim]))

(def team-players
  {1 [:player1 :player3 :player5]
   2 [:player2 :player4 :player6]})

(def default-checkpoint-file "target/karbosh-personality-checkpoints.edn")

(defn persona-by-name [name]
  (or (some #(when (= name (:name %)) %) room/bot-personas)
      (throw (ex-info "Unknown bot persona" {:name name}))))

(defn profile-label [play-strategies]
  (keyword (str/join "__" (map name play-strategies))))

(defn strategy-profile
  ([strategy]
   (strategy-profile (keyword (name strategy)) [strategy strategy strategy]))
  ([label play-strategies]
   {:label label
    :play-strategies (vec play-strategies)}))

(defn persona-profile [label names]
  (let [personas (mapv persona-by-name names)]
    {:label label
     :personas names
     :style-by-seat (mapv :style personas)
     :play-strategies (mapv :play-strategy personas)}))

(def default-profiles
  [(persona-profile :steady-preservation
                    ["Deal-E"
                     "Sir Shufflesworth"
                     "The Bid Lebowski"])
   (persona-profile :aggressive-trumpers
                    ["Trumpelstiltskin"
                     "Cardi-Bot"
                     "Bid Zeppelin"])
   (persona-profile :ruff-invite-table
                    ["HAL 52"
                     "Queen Latifah-Bot"
                     "Heart Vader"])
   (persona-profile :chaos-ruff-invite
                    ["Bender the Rules"
                     "Botzilla"
                     "Spade Invader"])
   (strategy-profile :all-hybrid-preservation
                     [:hybrid-preservation :hybrid-preservation :hybrid-preservation])
   (strategy-profile :all-hybrid
                     [:hybrid :hybrid :hybrid])
   (strategy-profile :all-hybrid-ruff-invite
                     [:hybrid-ruff-invite :hybrid-ruff-invite :hybrid-ruff-invite])
   (strategy-profile :mixed-preserve-ruff
                     [:hybrid-preservation :hybrid-ruff-invite :hybrid-ruff-invite])
   (strategy-profile :mixed-aggressive-ruff
                     [:hybrid :hybrid-ruff-invite :hybrid-ruff-invite])
   (strategy-profile :defender-exit-mix
                     [:hybrid-defender-exit :hybrid-preservation :hybrid-ruff-invite])])

(defn all-strategy-profiles []
  (->> (keys bot/play-strategies)
       sort
       (mapv strategy-profile)))

(defn profiles-by-label [profiles]
  (into {} (map (juxt :label identity) profiles)))

(defn parse-labels [s]
  (->> (str/split (or s "") #",")
       (map str/trim)
       (remove str/blank?)
       (mapv keyword)))

(defn select-profiles [profile-set labels]
  (let [profiles (case profile-set
                   :all-strategies (all-strategy-profiles)
                   :default default-profiles)
        by-label (profiles-by-label profiles)]
    (if (seq labels)
      (mapv (fn [label]
              (or (get by-label label)
                  (throw (ex-info "Unknown profile label"
                                  {:label label
                                   :available (keys by-label)}))))
            labels)
      profiles)))

(defn team-play-strategy-by-player [team profile]
  (zipmap (get team-players team)
          (:play-strategies profile)))

(defn profile-pairs [profiles]
  (let [profiles (vec profiles)]
    (for [i (range (count profiles))
          j (range (inc i) (count profiles))]
      [(nth profiles i) (nth profiles j)])))

(def matchup-orientations [:forward :reverse])

(defn matchup-jobs [profiles seeds]
  (for [seed seeds
        [left right] (profile-pairs profiles)
        orientation matchup-orientations]
    {:seed seed
     :left left
     :right right
     :orientation orientation}))

(defn assignment [{:keys [left right orientation]}]
  (if (= :forward orientation)
    {:profile-by-team {1 left 2 right}}
    {:profile-by-team {1 right 2 left}}))

(defn play-strategy-by-player [profile-by-team]
  (merge (team-play-strategy-by-player 1 (get profile-by-team 1))
         (team-play-strategy-by-player 2 (get profile-by-team 2))))

(defn winner-label [profile-by-team result]
  (some-> result
          :winner
          profile-by-team
          :label))

(defn run-matchup-job [options {:keys [seed left right orientation] :as job}]
  (let [{:keys [profile-by-team]} (assignment job)
        result (sim/summarize-game
                (sim/run-game seed
                              (assoc options
                                :play-strategy-by-player
                                (play-strategy-by-player profile-by-team))))]
    (assoc result
           :seed seed
           :orientation orientation
           :profiles [(:label left) (:label right)]
           :profile-by-team (update-vals profile-by-team :label)
           :policy-winner (winner-label profile-by-team result))))

(defn empty-accumulator []
  {:games 0
   :hands 0
   :appearances {}
   :wins {}
   :stop-reasons {}
   :matchups {}})

(defn add-appearance [acc label]
  (update-in acc [:appearances label] (fnil inc 0)))

(defn add-win [acc label]
  (if label
    (update-in acc [:wins label] (fnil inc 0))
    acc))

(defn sorted-matchup-key [labels]
  (vec (sort-by name labels)))

(defn add-matchup-result [acc {:keys [profiles policy-winner]}]
  (let [k (sorted-matchup-key profiles)]
    (-> acc
        (update-in [:matchups k :games] (fnil inc 0))
        (update-in [:matchups k :wins (or policy-winner :unresolved)] (fnil inc 0)))))

(defn add-result [acc {:keys [profiles policy-winner hands stop-reason] :as result}]
  (let [acc (-> acc
                (update :games inc)
                (update :hands + (or hands 0))
                (update-in [:stop-reasons stop-reason] (fnil inc 0)))
        acc (reduce add-appearance acc profiles)]
    (-> acc
        (add-win policy-winner)
        (add-matchup-result result))))

(defn rate [n total]
  (if (pos? total)
    (double (/ n total))
    0.0))

(defn profile-row [previous-rates {:keys [appearances wins]} label]
  (let [games (get appearances label 0)
        win-rate (rate (get wins label 0) games)
        previous (get previous-rates label)]
    (cond-> {:label label
             :games games
             :wins (get wins label 0)
             :win-rate win-rate}
      previous
      (assoc :derivative (- win-rate previous)))))

(defn rankings [labels previous-rates acc]
  (->> labels
       (map #(profile-row previous-rates acc %))
       (sort-by (juxt (comp - :win-rate)
                      (comp - :wins)
                      :label))
       vec))

(defn win-rate-map [rows]
  (into {} (map (juxt :label :win-rate) rows)))

(defn max-abs-derivative [rows]
  (reduce max 0.0 (map #(Math/abs (double (:derivative % 0.0))) rows)))

(defn profile-definition [{:keys [label personas style-by-seat play-strategies]}]
  (cond-> {:label label
           :play-strategies play-strategies}
    personas
    (assoc :personas personas)

    style-by-seat
    (assoc :style-by-seat style-by-seat)))

(defn matchup-rows [matchups]
  (->> matchups
       (map (fn [[profiles {:keys [games wins]}]]
              {:profiles profiles
               :games games
               :wins wins
               :win-rates (into {}
                                (map (fn [[label n]]
                                       [label (rate n games)]))
                                wins)}))
       (sort-by (juxt :profiles))
       vec))

(defn checkpoint-summary [checkpoint labels previous-rates acc batch results]
  (let [rows (rankings labels previous-rates acc)]
    {:checkpoint checkpoint
     :seed-range [(first batch) (last batch)]
     :batch-seeds (count batch)
     :batch-games (count results)
     :games (:games acc)
     :hands (:hands acc)
     :avg-hands (rate (:hands acc) (:games acc))
     :stop-reasons (:stop-reasons acc)
     :rankings rows
     :win-rates (win-rate-map rows)
     :matchups (matchup-rows (:matchups acc))
     :max-abs-derivative (max-abs-derivative rows)}))

(defn run-batch [profiles options seeds]
  (doall (pmap #(run-matchup-job options %)
               (matchup-jobs profiles seeds))))

(defn tournament-steps
  "Lazy checkpoint stream. Every checkpoint runs all profile pairs for a seed
  batch, with each pair mirrored across both teams to reduce seating bias."
  ([profiles seeds]
   (tournament-steps profiles seeds sim/default-options {}))
  ([profiles seeds options {:keys [checkpoint-seeds]
                            :or {checkpoint-seeds 25}}]
   (let [profiles (vec profiles)
         labels (mapv :label profiles)
         batches (partition-all checkpoint-seeds seeds)]
     ((fn step [checkpoint acc previous-rates batches]
        (lazy-seq
         (when-let [batch (seq (first batches))]
         (let [batch (vec batch)
               results (run-batch profiles options batch)
               acc' (reduce add-result acc results)
               summary (checkpoint-summary checkpoint
                                           labels
                                           previous-rates
                                           acc'
                                           batch
                                           results)]
             (cons (assoc summary
                          :profile-definitions (mapv profile-definition profiles))
                   (step (inc checkpoint)
                         acc'
                         (:win-rates summary)
                         (rest batches)))))))
      1
      (empty-accumulator)
      {}
      batches))))

(defn seed-seq [start seed-count]
  (let [seeds (iterate inc start)]
    (if seed-count
      (take seed-count seeds)
      seeds)))

(defn parse-long-option [x]
  (Long/parseLong x))

(defn parse-keyword-option [x]
  (keyword x))

(def option-parsers
  {"--start-seed" [:start-seed parse-long-option]
   "--seed-count" [:seed-count #(when-not (= "forever" %) (parse-long-option %))]
   "--checkpoint-seeds" [:checkpoint-seeds parse-long-option]
   "--max-hands" [:max-hands parse-long-option]
   "--min-score" [:min-score parse-long-option]
   "--top" [:top parse-long-option]
   "--profile-set" [:profile-set parse-keyword-option]
   "--profiles" [:profiles parse-labels]
   "--checkpoint-file" [:checkpoint-file identity]})

(def default-cli-options
  {:start-seed 0
   :seed-count nil
   :checkpoint-seeds 25
   :max-hands 100
   :min-score -100
   :top 12
   :profile-set :default
   :profiles []
   :checkpoint-file default-checkpoint-file})

(defn usage []
  (str/join
   "\n"
   ["Usage: clojure -M:karbosh-personality-sim [options]"
    ""
    "Options:"
    "  --seed-count N|forever       Number of seeds to run. Default: forever"
    "  --checkpoint-seeds N         Seeds per checkpoint. Default: 25"
    "  --start-seed N               First seed. Default: 0"
    "  --max-hands N                Simulation hand guard. Default: 100"
    "  --min-score N                Simulation negative score guard. Default: -100"
    "  --top N                      Rankings printed per checkpoint. Default: 12"
    "  --profile-set default|all-strategies"
    "  --profiles a,b,c             Optional profile labels from the chosen set"
    "  --checkpoint-file PATH       EDN-lines checkpoint output"
    ""
    "The run is lazy and checkpointed; use Ctrl-C to stop an unbounded run."]))

(defn parse-args [args]
  (loop [opts default-cli-options
         args args]
    (cond
      (empty? args)
      opts

      (#{"--help" "-h"} (first args))
      (assoc opts :help? true)

      :else
      (let [[flag value & more] args
            [k parser] (get option-parsers flag)]
        (when-not k
          (throw (ex-info "Unknown option" {:flag flag})))
        (when-not value
          (throw (ex-info "Missing option value" {:flag flag})))
        (recur (assoc opts k (parser value)) more)))))

(defn print-checkpoint! [{:keys [top]} {:keys [checkpoint games hands
                                               avg-hands max-abs-derivative
                                               rankings]}]
  (println)
  (println (format "Checkpoint %d | games=%d | hands=%d | avg-hands=%.2f | max-d=%.5f"
                   checkpoint
                   games
                   hands
                   (double avg-hands)
                   (double max-abs-derivative)))
  (doseq [{:keys [label games wins win-rate derivative]}
          (take top rankings)]
    (println (format "  %-32s games=%-6d wins=%-6d win-rate=%6.2f%% d=% .5f"
                     (name label)
                     games
                     wins
                     (* 100.0 win-rate)
                     (double (or derivative 0.0))))))

(defn append-checkpoint! [file checkpoint]
  (when (seq file)
    (io/make-parents file)
    (spit file (str (pr-str checkpoint) "\n") :append true)))

(defn run-cli! [opts]
  (let [profiles (select-profiles (:profile-set opts) (:profiles opts))]
    (when (< (count profiles) 2)
      (throw (ex-info "Need at least two profiles" {:profiles (mapv :label profiles)})))
    (println "Karbosh personality tournament")
    (println "Profiles:" (str/join ", " (map (comp name :label) profiles)))
    (println "Checkpoint file:" (:checkpoint-file opts))
    (doseq [checkpoint (tournament-steps
                        profiles
                        (seed-seq (:start-seed opts) (:seed-count opts))
                        (assoc sim/default-options
                          :max-hands (:max-hands opts)
                          :min-score (:min-score opts))
                        {:checkpoint-seeds (:checkpoint-seeds opts)})]
      (print-checkpoint! opts checkpoint)
      (append-checkpoint! (:checkpoint-file opts) checkpoint))))

(defn -main [& args]
  (let [opts (parse-args args)]
    (if (:help? opts)
      (println (usage))
      (try
        (run-cli! opts)
        (finally
          (shutdown-agents))))))
