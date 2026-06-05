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

(defn state-key [state target-team]
  [target-team
   (:trump state)
   (active-players state)
   (:current-player state)
   (:current-trick state)
   (teams-key state)
   (into {}
         (map (fn [player]
                [player (hand-key state player)]))
         (active-players state))])

(defn solve-future-tricks
  "Return the number of future tricks `target-team` can force from a perfect
  information play state.

  Players on `target-team` maximize the value; all other players minimize it.
  Duplicate physical cards are collapsed by `rules/legal-cards`, so identical
  plays do not create duplicate branches."
  [state target-team]
  (let [cache (atom {})]
    (letfn [(solve* [state]
              (let [k (state-key state target-team)]
                (if-let [cached (find @cache k)]
                  (val cached)
                  (let [value
                        (if (terminal? state)
                          0
                          (let [player (:current-player state)
                                choose (if (= target-team (player-team state player))
                                         max
                                         min)
                                before (get-in state [:tricks-this-hand target-team] 0)
                                scores (map (fn [card]
                                              (let [next-state (play-card state player card)
                                                    after (get-in next-state
                                                                  [:tricks-this-hand target-team]
                                                                  0)]
                                                (+ (- after before)
                                                   (solve* next-state))))
                                            (legal-plays state))]
                            (if (seq scores)
                              (apply choose scores)
                              0)))]
                    (swap! cache assoc k value)
                    value))))]
      (solve* state))))
