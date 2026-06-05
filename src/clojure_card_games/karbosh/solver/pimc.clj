(ns clojure-card-games.karbosh.solver.pimc
  (:require [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.solver.play :as play]
            [clojure-card-games.karbosh.solver.sample :as sample]))

(def default-samples 100)

(defn players-for [state players]
  (vec (or players (play/active-players state))))

(defn state-known-hands [state players]
  (into {}
        (map (fn [player]
               [player (vec (get-in state [:players player :hand] []))]))
        players))

(defn current-trick-cards [state]
  (map :card (:current-trick state)))

(defn ensure-player [state player]
  (update-in state [:players player]
             (fn [seat]
               (merge {:team (get (game/teams) player)
                       :hand []}
                      seat))))

(defn with-sampled-hands [state hands]
  (reduce-kv (fn [state player hand]
               (-> state
                   (ensure-player player)
                   (assoc-in [:players player :hand] hand)))
             state
             hands))

(defn sample-state
  "Produce one perfect-information world from a partial information state.

  Known hands are read from `state`; unknown cards are sampled from the deck
  after removing known hands plus visible cards such as the current trick."
  [state {:keys [seed players hand-size hand-sizes known-cards deck]}]
  (let [players (players-for state players)
        hands (sample/sample-hands
                {:seed seed
                 :players players
                 :hand-size (or hand-size 8)
                 :hand-sizes hand-sizes
                 :known-hands (state-known-hands state players)
                 :known-cards (concat known-cards
                                      (current-trick-cards state))
                 :deck deck})]
    (with-sampled-hands state hands)))

(defn seeds-for [{:keys [seeds samples seed]
                  :or {samples default-samples
                       seed 0}}]
  (vec (or seeds (range seed (+ seed samples)))))

(defn mean [xs]
  (when (seq xs)
    (/ (reduce + xs) (double (count xs)))))

(defn downside-values [values]
  (filter neg? values))

(defn summarize-outcomes [outcomes]
  (let [values (mapv :value outcomes)
        made (count (filter :made? outcomes))
        losses (downside-values values)]
    {:samples (count outcomes)
     :made made
     :failed (- (count outcomes) made)
     :make-rate (if (seq outcomes)
                  (/ made (double (count outcomes)))
                  0.0)
     :ev (or (mean values) 0.0)
     :downside-rate (if (seq outcomes)
                      (/ (count losses) (double (count outcomes)))
                      0.0)
     :avg-downside (or (mean losses) 0.0)
     :worst (when (seq values) (apply min values))
     :best (when (seq values) (apply max values))}))

(defn evaluate-contract
  "Evaluate a candidate contract with Perfect Information Monte Carlo.

  Each sampled world fills hidden hands, solves optimal perfect-information
  play for the contract, and records the bidding team's point differential."
  ([state contract] (evaluate-contract state contract {}))
  ([state contract options]
   (let [outcomes (mapv (fn [seed]
                          (let [world (sample-state state (assoc options :seed seed))
                                value (play/solve-contract world contract)]
                            {:seed seed
                             :value value
                             :made? (pos? value)}))
                        (seeds-for options))]
     (assoc (summarize-outcomes outcomes)
            :contract contract
            :outcomes outcomes))))
