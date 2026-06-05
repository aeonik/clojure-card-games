(ns clojure-card-games.karbosh.solver.play
  (:require [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(defn active-players [state]
  (game/trick-players state))

(defn player-team [state player]
  (get-in state [:players player :team]))

(defn hand [state player]
  (get-in state [:players player :hand] []))

(defn legal-plays [state]
  (let [player (:current-player state)]
    (rules/legal-cards (hand state player)
                       (:current-trick state)
                       (:trump state))))

(defn terminal? [state]
  (and (empty? (:current-trick state))
       (every? #(empty? (hand state %)) (active-players state))))

(defn- remove-card [cards card]
  (game/remove-first card cards))

(defn play-card [state player card]
  (let [trick (conj (:current-trick state) {:player player :card card})
        state (update-in state [:players player :hand] remove-card card)]
    (if (= (count trick) (count (active-players state)))
      (let [winner (rules/resolve-trick trick (:trump state))
            winner-team (player-team state winner)]
        (-> state
            (update-in [:tricks-this-hand winner-team] (fnil inc 0))
            (assoc :current-trick []
                   :trick-leader winner
                   :current-player winner)))
      (assoc state
             :current-trick trick
             :current-player (game/next-trick-player state player)))))

(defn- hand-key [state player]
  (frequencies (hand state player)))

(defn- teams-key [state]
  (into {}
        (map (fn [player]
               [player (player-team state player)]))
        (active-players state)))

(defn state-key [state objective]
  [objective
   (:trump state)
   (active-players state)
   (:current-player state)
   (:current-trick state)
   (:tricks-this-hand state)
   (teams-key state)
   (into {}
         (map (fn [player]
                [player (hand-key state player)]))
         (active-players state))])

(defn other-team [team]
  (case team
    1 2
    2 1))

(defn team-differential [points team]
  (- (get points team 0)
     (get points (other-team team) 0)))

(defn contract-team [state contract]
  (player-team state (:player contract)))

(defn contract-points [state contract]
  (rules/score-hand (:players state)
                    contract
                    (:tricks-this-hand state)))

(defn contract-utility
  "Return point differential from the bidding team's perspective."
  [state contract]
  (team-differential (contract-points state contract)
                     (contract-team state contract)))

(defn maximizing-player? [state max-team]
  (= max-team (player-team state (:current-player state))))

(defn move-priority [state max-team card]
  (let [player (:current-player state)
        trump (:trump state)
        trick (conj (:current-trick state) {:player player :card card})
        lead (or (rules/trick-lead (:current-trick state) trump)
                 (rules/effective-suit card trump))
        winner (rules/resolve-trick trick trump)
        winner-team (player-team state winner)
        complete? (= (count trick) (count (active-players state)))
        trick-value (cond
                      (and complete? (= max-team winner-team)) 100000
                      complete? -100000
                      (= max-team winner-team) 10000
                      :else -10000)]
    (+ trick-value (rules/card-value card trump lead))))

(defn ordered-plays [state max-team]
  (let [plays (legal-plays state)
        order (if (maximizing-player? state max-team) > <)]
    (sort-by #(move-priority state max-team %) order plays)))

(defn solve-value-exhaustive
  "Solve a perfect-information play state with a terminal evaluator.

  `max-team` chooses actions that maximize the terminal value. All other teams
  choose actions that minimize it."
  [state {:keys [objective max-team terminal-value]}]
  (let [cache (atom {})]
    (letfn [(solve* [state]
              (let [k (state-key state objective)]
                (if-let [cached (find @cache k)]
                  (val cached)
                  (let [value
                        (if (terminal? state)
                          (terminal-value state)
                          (let [player (:current-player state)
                                choose (if (= max-team (player-team state player))
                                         max
                                         min)
                                scores (map (fn [card]
                                              (solve* (play-card state player card)))
                                            (ordered-plays state max-team))]
                            (if (seq scores)
                              (apply choose scores)
                              (terminal-value state))))]
                    (swap! cache assoc k value)
                    value))))]
      (solve* state))))

(def negative-infinity Double/NEGATIVE_INFINITY)
(def positive-infinity Double/POSITIVE_INFINITY)

(defn solve-value
  "Solve a perfect-information play state using alpha-beta pruning.

  The public result is still an exact minimax value. Cutoff nodes are not
  stored in the exact transposition cache."
  [state {:keys [objective max-team terminal-value]}]
  (let [cache (atom {})]
    (letfn [(result [value exact?]
              {:value value :exact? exact?})
            (search-children [k state plays max? best alpha beta exact?]
              (if-let [card (first plays)]
                (let [child (solve*
                              (play-card state (:current-player state) card)
                              alpha
                              beta)
                      value (:value child)
                      best (if max?
                             (max best value)
                             (min best value))
                      alpha (if max?
                              (max alpha best)
                              alpha)
                      beta (if max?
                             beta
                             (min beta best))
                      exact? (and exact? (:exact? child))]
                  (if (if max?
                        (>= alpha beta)
                        (<= beta alpha))
                    (result best false)
                    (recur k
                           state
                           (rest plays)
                           max?
                           best
                           alpha
                           beta
                           exact?)))
                (do
                  (when exact?
                    (swap! cache assoc k best))
                  (result best exact?))))
            (solve* [state alpha beta]
              (let [k (state-key state objective)]
                (cond
                  (terminal? state)
                  (result (terminal-value state) true)

                  (contains? @cache k)
                  (result (get @cache k) true)

                  :else
                  (let [plays (seq (ordered-plays state max-team))]
                    (if-not plays
                      (result (terminal-value state) true)
                      (let [max? (maximizing-player? state max-team)
                            initial-best (if max?
                                           negative-infinity
                                           positive-infinity)]
                        (search-children k
                                         state
                                         plays
                                         max?
                                         initial-best
                                         alpha
                                         beta
                                         true)))))))]
      (:value (solve* state negative-infinity positive-infinity)))))

(defn solve-future-tricks
  "Return the number of future tricks `target-team` can force from a perfect
  information play state.

  Players on `target-team` maximize the value; all other players minimize it.
  Duplicate physical cards are collapsed by `rules/legal-cards`, so identical
  plays do not create duplicate branches."
  [state target-team]
  (let [baseline (get-in state [:tricks-this-hand target-team] 0)]
    (- (solve-value state
                    {:objective [:future-tricks target-team]
                     :max-team target-team
                     :terminal-value #(get-in % [:tricks-this-hand target-team] 0)})
       baseline)))

(defn solve-contract
  "Return optimal contract point differential from the bidding team's
  perspective."
  [state contract]
  (let [team (contract-team state contract)]
    (solve-value state
                 {:objective [:contract contract]
                  :max-team team
                  :terminal-value #(contract-utility % contract)})))
