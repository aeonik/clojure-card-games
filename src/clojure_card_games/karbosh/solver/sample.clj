(ns clojure-card-games.karbosh.solver.sample
  (:require [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]))

(defn remove-one [cards card]
  (let [[before after] (split-with #(not= card %) cards)]
    (when-not (seq after)
      (throw (ex-info "Card is not available"
                      {:card card})))
    (vec (concat before (rest after)))))

(defn remove-cards [deck removed]
  (reduce remove-one (vec deck) removed))

(defn default-hand-sizes [players hand-size]
  (zipmap players (repeat hand-size)))

(defn hand-needs [players hand-sizes known-hands]
  (into {}
        (map (fn [player]
               (let [known-count (count (get known-hands player))
                     target-count (get hand-sizes player)]
                 (when (> known-count target-count)
                   (throw (ex-info "Known hand exceeds target size"
                                   {:player player
                                    :known-count known-count
                                    :target-count target-count})))
                 [player (- target-count known-count)]))
             players)))

(defn- deal-needed-cards [shoe needs]
  (loop [shoe shoe
         needs (seq needs)
         dealt {}]
    (if-let [[player n] (first needs)]
      (let [[cards shoe] (split-at n shoe)]
        (recur (vec shoe)
               (next needs)
               (assoc dealt player (vec cards))))
      dealt)))

(defn sample-hands
  "Deterministically sample complete hands from known cards and a seed.

  `known-hands` fixes known cards for any player. `known-cards` removes cards
  that are visible but no longer in a hand, such as played or discarded cards.
  `hand-sizes` describes the target hand size for each player in the sampled
  state; when omitted, every player targets `hand-size` cards."
  [{:keys [seed players hand-size hand-sizes known-hands known-cards deck]
    :or {players game/players
         hand-size 8
         known-hands {}
         known-cards []}}]
  (let [players (vec players)
        deck (or deck (cards/deck))
        hand-sizes (merge (default-hand-sizes players hand-size)
                          hand-sizes)
        known-hands (into {}
                          (map (fn [player]
                                 [player (vec (get known-hands player []))]))
                          players)
        needs (hand-needs players hand-sizes known-hands)
        removed (concat known-cards (mapcat known-hands players))
        shoe (cards/shuffle-deck (remove-cards deck removed) seed)
        needed (reduce + (vals needs))]
    (when-not (= needed (count shoe))
      (throw (ex-info "Hidden card count does not match requested hand sizes"
                      {:needed needed
                       :available (count shoe)})))
    (let [dealt (deal-needed-cards shoe needs)]
      (into {}
            (map (fn [player]
                   [player (vec (concat (get known-hands player)
                                        (get dealt player)))]))
            players))))
