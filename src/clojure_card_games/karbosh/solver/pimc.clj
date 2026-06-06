(ns clojure-card-games.karbosh.solver.pimc
  (:require [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.game :as game]
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

(defn summarize-make-outcomes [outcomes]
  (let [made (count (filter :made? outcomes))]
    {:samples (count outcomes)
     :made made
     :failed (- (count outcomes) made)
     :make-rate (if (seq outcomes)
                  (/ made (double (count outcomes)))
                  0.0)}))

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

(defn evaluate-contract-make
  "Evaluate make probability with Perfect Information Monte Carlo.

  Karbosh and Double Karbosh use the binary all-tricks solver, which can
  short-circuit after the defenders win any trick. Numeric contracts use the
  regular contract solver."
  ([state contract] (evaluate-contract-make state contract {}))
  ([state contract options]
   (let [outcomes (mapv (fn [seed]
                          (let [world (sample-state state (assoc options :seed seed))]
                            {:seed seed
                             :made? (play/solve-contract-made? world contract)}))
                        (seeds-for options))]
     (assoc (summarize-make-outcomes outcomes)
            :contract contract
            :outcomes outcomes))))

(defn remove-card-from-player [state player card]
  (update-in state [:players player :hand] #(game/remove-first card %)))

(defn add-card-to-player [state player card]
  (update-in state [:players player :hand] (fnil conj []) card))

(defn donate-card [state caller trump donor]
  (if-let [card (bot/strongest-card-for-trump
                  trump
                  (get-in state [:players donor :hand]))]
    (-> state
        (remove-card-from-player donor card)
        (add-card-to-player caller card)
        (update :donations (fnil conj [])
                {:type :donate-card
                 :player donor
                 :to caller
                 :card card}))
    state))

(defn prepare-karbosh-world
  "Apply the Karbosh setup phase to a perfect-information world.

  The caller discards its two weakest cards under trump, then each partner
  donates the strongest card it can see under that same trump. The resulting
  state starts trick play with the caller going alone against the opponents."
  [state contract trump]
  (let [caller (:player contract)
        discards (bot/karbosh-discard-cards
                   (get-in state [:players caller :hand])
                   trump)
        state (reduce #(remove-card-from-player %1 caller %2)
                      (assoc state
                             :trump trump
                             :current-trick []
                             :tricks-this-hand {1 0 2 0}
                             :donations []
                             :discards (mapv (fn [card]
                                               {:type :discard-card
                                                :player caller
                                                :card card})
                                             discards))
                      discards)
        state (reduce #(donate-card %1 caller trump %2)
                      state
                      (game/partner-players state caller))]
    (assoc state
           :phase :trick-playing
           :active-players (game/lone-hand-players state caller)
           :trick-leader caller
           :current-player caller)))

(defn evaluate-karbosh-donation-make
  "Evaluate Karbosh make probability from the pre-trump/pre-donation view.

  Each sample fills hidden hands, applies discard plus partner donation, then
  solves whether the caller can force every trick."
  ([state contract] (evaluate-karbosh-donation-make state contract {}))
  ([state contract options]
   (let [caller (:player contract)
         trump (or (:trump options)
                   (:trump state)
                   (bot/best-trump (get-in state [:players caller :hand])))
         outcomes (mapv (fn [seed]
                          (let [world (sample-state state
                                                    (assoc options :seed seed))
                                setup (prepare-karbosh-world world
                                                             contract
                                                             trump)]
                            {:seed seed
                             :trump trump
                             :discards (:discards setup)
                             :donations (:donations setup)
                             :made? (play/solve-karbosh-make?
                                      setup
                                      (assoc contract :bid-type :karbosh))}))
                        (seeds-for options))]
     (assoc (summarize-make-outcomes outcomes)
            :contract (assoc contract :bid-type :karbosh)
            :trump trump
            :outcomes outcomes))))
