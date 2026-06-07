(ns clojure-card-games.karbosh.logic.bench
  (:require [clojure.edn :as edn]
            [clojure-card-games.karbosh.logic.rules :as logic-rules]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(def trick-sizes [0 1 2 3 4 5])

(defn sample-context [seed trump trick-size]
  (let [deck (cards/shuffle-deck (cards/deck) seed)
        hand (vec (take 8 deck))
        trick-cards (take trick-size (drop 8 deck))
        trick-players (take trick-size game/players)
        trick (mapv (fn [player card]
                      {:player player :card card})
                    trick-players
                    trick-cards)]
    {:seed seed
     :trump trump
     :hand hand
     :trick trick}))

(defn sample-contexts [n]
  (mapv (fn [idx]
          (sample-context idx
                          (nth (cycle cards/suits) idx)
                          (nth (cycle trick-sizes) idx)))
        (range n)))

(defn pure-legal-cards [{:keys [hand trick trump]}]
  (rules/legal-cards hand trick trump))

(defn logic-legal-cards [{:keys [hand trick trump]}]
  (logic-rules/legal-cards hand trick trump))

(defn mismatch [context]
  (let [pure (set (pure-legal-cards context))
        logic (set (logic-legal-cards context))]
    (when (not= pure logic)
      (assoc context
             :pure pure
             :logic logic))))

(defn timed [f]
  (let [start (System/nanoTime)
        result (f)
        elapsed (- (System/nanoTime) start)]
    {:result result
     :elapsed-ns elapsed
     :elapsed-ms (/ elapsed 1000000.0)}))

(defn legal-card-count [f contexts]
  (reduce + (map #(count (f %)) contexts)))

(defn bench
  ([] (bench 1000))
  ([n]
   (let [contexts (sample-contexts n)
         warmup (legal-card-count logic-legal-cards (take (min 25 n) contexts))
         pure (timed #(legal-card-count pure-legal-cards contexts))
         logic (timed #(legal-card-count logic-legal-cards contexts))
         mismatches (vec (keep mismatch contexts))
         ratio (when (pos? (:elapsed-ns pure))
                 (double (/ (:elapsed-ns logic) (:elapsed-ns pure))))]
     {:contexts n
      :warmup-card-count warmup
      :pure {:card-count (:result pure)
             :elapsed-ms (:elapsed-ms pure)}
      :core-logic {:card-count (:result logic)
                   :elapsed-ms (:elapsed-ms logic)}
      :logic-over-pure-ratio ratio
      :mismatch-count (count mismatches)
      :first-mismatch (first mismatches)})))

(defn parse-n [args]
  (if-let [s (first args)]
    (let [n (edn/read-string s)]
      (if (pos-int? n) n 1000))
    1000))

(defn -main [& args]
  (println (pr-str (bench (parse-n args)))))
