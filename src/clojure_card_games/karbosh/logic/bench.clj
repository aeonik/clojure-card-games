(ns clojure-card-games.karbosh.logic.bench
  (:require [clojure.core.logic :as l]
            [clojure.edn :as edn]
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

(defn current-player [{:keys [trick]}]
  (nth game/players (count trick)))

(defn teams []
  (game/teams))

(defn card-score [context card]
  (let [trump (:trump context)
        lead (or (rules/trick-lead (:trick context) trump)
                 (rules/effective-suit card trump))]
    (rules/card-value card trump lead)))

(defn lowest-card [context cards]
  (first (sort-by #(card-score context %) cards)))

(defn same-team? [a b]
  (and a b (= (get (teams) a) (get (teams) b))))

(defn pure-legal-cards [{:keys [hand trick trump]}]
  (rules/legal-cards hand trick trump))

(defn logic-legal-cards [{:keys [hand trick trump]}]
  (logic-rules/legal-cards hand trick trump))

(defn pure-effective-suits [{:keys [hand trick trump]}]
  (mapv #(rules/effective-suit % trump) (concat hand (map :card trick))))

(defn logic-effective-suits [{:keys [hand trick trump]}]
  (mapv (fn [card]
          (first (l/run 1 [suit]
                   (logic-rules/effective-suito card trump suit))))
        (concat hand (map :card trick))))

(defn pure-winning-play [{:keys [trick trump]}]
  (rules/winning-play trick trump))

(defn logic-winning-play [{:keys [trick trump]}]
  (logic-rules/winning-play trick trump))

(defn winning-cards [legal-cards winning-play-fn context]
  (let [player (current-player context)
        trick (:trick context)
        trump (:trump context)]
    (filter #(= player (:player (winning-play-fn
                                  (assoc context
                                         :trick (conj trick {:player player
                                                             :card %})
                                         :trump trump))))
            legal-cards)))

(defn simple-card-action [legal-card-fn winning-play-fn context]
  (let [player (current-player context)
        cards (vec (legal-card-fn context))
        winner (:player (winning-play-fn context))
        winning-cards (vec (winning-cards cards winning-play-fn context))
        card (cond
               (empty? cards)
               nil

               (empty? (:trick context))
               (lowest-card context cards)

               (same-team? player winner)
               (lowest-card context cards)

               (seq winning-cards)
               (lowest-card context winning-cards)

               :else
               (lowest-card context cards))]
    (when card
      {:type :play-card
       :card card})))

(defn pure-simple-card-action [context]
  (simple-card-action pure-legal-cards pure-winning-play context))

(defn logic-simple-card-action [context]
  (simple-card-action logic-legal-cards logic-winning-play context))

(defn mismatch [pure-fn logic-fn normalize context]
  (let [pure (normalize (pure-fn context))
        logic (normalize (logic-fn context))]
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

(defn output-hash [f contexts]
  (hash (mapv f contexts)))

(def tasks
  [{:name :effective-suit
    :pure pure-effective-suits
    :logic logic-effective-suits
    :measure output-hash
    :normalize identity}
   {:name :legal-cards
    :pure pure-legal-cards
    :logic logic-legal-cards
    :measure legal-card-count
    :normalize set}
   {:name :winning-play
    :pure pure-winning-play
    :logic logic-winning-play
    :measure output-hash
    :normalize identity}
   {:name :simple-card-action
    :pure pure-simple-card-action
    :logic logic-simple-card-action
    :measure output-hash
    :normalize identity}])

(defn benchmark-task [contexts {:keys [name pure logic measure normalize]}]
  (measure logic (take (min 25 (count contexts)) contexts))
  (let [pure-result (timed #(measure pure contexts))
        logic-result (timed #(measure logic contexts))
        mismatches (vec (keep #(mismatch pure logic normalize %) contexts))
        ratio (when (pos? (:elapsed-ns pure-result))
                (double (/ (:elapsed-ns logic-result)
                           (:elapsed-ns pure-result))))]
    {:task name
     :pure {:result (:result pure-result)
            :elapsed-ms (:elapsed-ms pure-result)}
     :core-logic {:result (:result logic-result)
                  :elapsed-ms (:elapsed-ms logic-result)}
     :logic-over-pure-ratio ratio
     :mismatch-count (count mismatches)
     :first-mismatch (first mismatches)}))

(defn bench
  ([] (bench 1000))
  ([n]
   (let [contexts (sample-contexts n)
         task-results (mapv #(benchmark-task contexts %) tasks)]
     {:contexts n
      :tasks task-results})))

(defn parse-n [args]
  (if-let [s (first args)]
    (let [n (edn/read-string s)]
      (if (pos-int? n) n 1000))
    1000))

(defn -main [& args]
  (println (pr-str (bench (parse-n args)))))
