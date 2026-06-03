(ns clojure-card-games.karbosh.bot
  (:require [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(defn suit-strength [hand suit]
  (reduce + (map #(rules/card-value % suit suit) hand)))

(defn best-trump [hand]
  (apply max-key #(suit-strength hand %) cards/suits))

(def default-bid-config
  {:bid-4-strength 3000
   :bid-5-strength 4300
   :bid-6-strength 6500
   :high-trump-strength 600
   :karbosh-min-trumps 6
   :karbosh-min-high-trumps 5
   :karbosh-min-winners 7})

(def ^:dynamic *bid-config* default-bid-config)

(defn trump-card? [trump card]
  (= trump (rules/effective-suit card trump)))

(defn high-trump? [config trump card]
  (>= (rules/card-value card trump trump)
      (:high-trump-strength config)))

(defn off-ace? [trump [rank :as card]]
  (and (= :A rank)
       (not (trump-card? trump card))))

(defn karbosh-hand? [config hand trump]
  (let [trumps (filter #(trump-card? trump %) hand)
        high-trumps (filter #(high-trump? config trump %) trumps)
        winners (+ (count high-trumps)
                   (count (filter #(off-ace? trump %) hand)))]
    (and (some #(rules/right-bower? % trump) hand)
         (>= (count trumps) (:karbosh-min-trumps config))
         (>= (count high-trumps) (:karbosh-min-high-trumps config))
         (>= winners (:karbosh-min-winners config)))))

(defn target-bid
  ([hand] (target-bid *bid-config* hand))
  ([config hand]
   (let [trump (best-trump hand)
         strength (suit-strength hand trump)]
     (cond
       (karbosh-hand? config hand trump) {:type :bid :bid-type :karbosh}
       (>= strength (:bid-6-strength config)) {:type :bid :bid-type :bid :value 6}
       (>= strength (:bid-5-strength config)) {:type :bid :bid-type :bid :value 5}
       (>= strength (:bid-4-strength config)) {:type :bid :bid-type :bid :value 4}
       :else {:type :bid :bid-type :pass}))))

(defn bid-action [game player]
  (let [candidate (target-bid (get-in game [:players player :hand]))
        current-rank (rules/bid-rank (game/current-bid game))]
    (if (> (rules/bid-rank candidate) current-rank)
      candidate
      {:type :bid :bid-type :pass})))

(defn trump-action [game player]
  {:type :trump-selection
   :suit (best-trump (get-in game [:players player :hand]))})

(declare highest-card lowest-card)

(defn donate-action [game player]
  (when-let [card (highest-card game (get-in game [:players player :hand]))]
    {:type :donate-card
     :card card}))

(defn discard-action [game player]
  (when-let [card (lowest-card game (get-in game [:players player :hand]))]
    {:type :discard-card
     :card card}))

(defn legal-cards [game player]
  (let [hand (get-in game [:players player :hand])]
    (filter #(rules/legal-play? hand (:current-trick game) % (:trump game)) hand)))

(defn card-score [game card]
  (let [lead (or (some-> (:current-trick game) first :card
                         (rules/effective-suit (:trump game)))
                 (rules/effective-suit card (:trump game)))]
    (rules/card-value card (:trump game) lead)))

(defn current-trick-winner [game]
  (when (seq (:current-trick game))
    (rules/resolve-trick (:current-trick game) (:trump game))))

(defn same-team? [game a b]
  (and a b (= (get-in game [:players a :team])
              (get-in game [:players b :team]))))

(defn wins-trick? [game player card]
  (= player (rules/resolve-trick (conj (:current-trick game)
                                       {:player player :card card})
                                 (:trump game))))

(defn lowest-card [game cards]
  (first (sort-by #(card-score game %) cards)))

(defn highest-card [game cards]
  (first (sort-by #(card-score game %) > cards)))

(defn card-action [game player]
  (let [cards (vec (legal-cards game player))
        winner (current-trick-winner game)
        winning-cards (filter #(wins-trick? game player %) cards)
        card (cond
               (empty? cards)
               nil

               (empty? (:current-trick game))
               (highest-card game cards)

               (same-team? game player winner)
               (lowest-card game cards)

               (seq winning-cards)
               (lowest-card game winning-cards)

               :else
               (lowest-card game cards))]
    (when card
      {:type :play-card
       :card card})))

(defn action [game player]
  (case (:phase game)
    :bidding (bid-action game player)
    :trump-selection (trump-action game player)
    :karbosh-donation (donate-action game player)
    :karbosh-discard (discard-action game player)
    :trick-playing (card-action game player)
    nil))
