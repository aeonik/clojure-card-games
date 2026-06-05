(ns clojure-card-games.karbosh.bot
  (:require [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.karbosh.shared.cards :as cards]
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
    (rules/legal-cards hand (:current-trick game) (:trump game))))

(defn completed-trick-cards [game]
  (analysis/completed-trick-cards game))

(defn current-trick-cards [game]
  (analysis/current-trick-cards game))

(defn public-played-cards [game]
  (analysis/public-played-cards game))

(defn seen-cards [game player]
  (analysis/seen-cards game player))

(defn unseen-cards [game player]
  (analysis/unseen-cards game player))

(defn unseen-card-counts [game player]
  (analysis/unseen-card-counts game player))

(defn hypergeom-analysis [game player]
  (analysis/player-analysis game player))

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

(defn can-be-beaten-by? [game unseen-counts card lead]
  (let [trump (:trump game)
        value (rules/card-value card trump lead)]
    (some (fn [[hidden-card n]]
            (and (pos? n)
                 (> (rules/card-value hidden-card trump lead) value)))
          unseen-counts)))

(defn can-be-beaten? [game player card lead]
  (can-be-beaten-by? game (unseen-card-counts game player) card lead))

(defn good-card-with-counts? [game unseen-counts card]
  (let [lead (or (rules/trick-lead (:current-trick game) (:trump game))
                 (rules/effective-suit card (:trump game)))]
    (not (can-be-beaten-by? game unseen-counts card lead))))

(defn good-card? [game player card]
  (good-card-with-counts? game (unseen-card-counts game player) card))

(defn secure-winning-card-with-counts? [game player unseen-counts card]
  (and (wins-trick? game player card)
       (good-card-with-counts? game unseen-counts card)))

(defn secure-winning-card? [game player card]
  (secure-winning-card-with-counts? game
                                    player
                                    (unseen-card-counts game player)
                                    card))

(defn lowest-card [game cards]
  (first (sort-by #(card-score game %) cards)))

(defn highest-card [game cards]
  (first (sort-by #(card-score game %) > cards)))

(defn card-action [game player]
  (let [cards (vec (legal-cards game player))
        winner (current-trick-winner game)
        unseen-counts (unseen-card-counts game player)
        winning-cards (filter #(wins-trick? game player %) cards)
        good-cards (filter #(good-card-with-counts? game unseen-counts %) cards)
        secure-winning-cards (filter #(secure-winning-card-with-counts?
                                        game
                                        player
                                        unseen-counts
                                        %)
                                     winning-cards)
        card (cond
               (empty? cards)
               nil

               (empty? (:current-trick game))
               (if (seq good-cards)
                 (lowest-card game good-cards)
                 (highest-card game cards))

               (same-team? game player winner)
               (lowest-card game cards)

               (seq secure-winning-cards)
               (lowest-card game secure-winning-cards)

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
