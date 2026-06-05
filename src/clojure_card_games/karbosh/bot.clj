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
   :karbosh-min-high-trumps 6
   :karbosh-min-bowers 2
   :karbosh-min-winners 7})

(def ^:dynamic *bid-config* default-bid-config)

(def default-play-config
  {:lead-risk-tolerance 0.32
   :win-risk-tolerance 0.22
   :lead-risk-penalty 900
   :win-risk-penalty 700
   :karbosh-lead-risk-tolerance 0.03
   :karbosh-win-risk-tolerance 0.01
   :karbosh-lead-risk-penalty 6500
   :karbosh-win-risk-penalty 6500})

(def default-play-strategy :hybrid)

(def ^:dynamic *play-config* default-play-config)
(def ^:dynamic *play-strategy* default-play-strategy)

(defn trump-card? [trump card]
  (= trump (rules/effective-suit card trump)))

(defn high-trump? [config trump card]
  (>= (rules/card-value card trump trump)
      (:high-trump-strength config)))

(defn off-ace? [trump [rank :as card]]
  (and (= :A rank)
       (not (trump-card? trump card))))

(defn bower? [trump card]
  (or (rules/right-bower? card trump)
      (rules/left-bower? card trump)))

(defn karbosh-hand? [config hand trump]
  (let [trumps (filter #(trump-card? trump %) hand)
        high-trumps (filter #(high-trump? config trump %) trumps)
        bowers (filter #(bower? trump %) trumps)
        winners (+ (count high-trumps)
                   (count (filter #(off-ace? trump %) hand)))]
    (and (some #(rules/right-bower? % trump) hand)
         (>= (count trumps) (:karbosh-min-trumps config))
         (>= (count high-trumps) (:karbosh-min-high-trumps config))
         (>= (count bowers) (:karbosh-min-bowers config))
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

(def special-bid-types #{:karbosh :double-karbosh})

(defn special-contract? [bid]
  (contains? special-bid-types (:bid-type bid)))

(defn special-contract-caller? [game player]
  (let [bid (game/current-bid game)]
    (and (special-contract? bid)
         (= player (:player bid)))))

(defn context-play-config [config game player]
  (if (special-contract-caller? game player)
    (assoc config
           :lead-risk-tolerance (:karbosh-lead-risk-tolerance config)
           :win-risk-tolerance (:karbosh-win-risk-tolerance config)
           :lead-risk-penalty (:karbosh-lead-risk-penalty config)
           :win-risk-penalty (:karbosh-win-risk-penalty config))
    config))

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

(defn card-counting-card-action [game player]
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

(defn card-analyses [game player cards]
  (let [trump (:trump game)
        unseen (vec (unseen-cards game player))
        population-size (count unseen)
        counts (analysis/effective-suit-counts trump unseen)]
    (into {}
          (map (fn [card]
                 [card
                  (analysis/card-defeat-analysis game
                                                 player
                                                 trump
                                                 unseen
                                                 counts
                                                 population-size
                                                 card)]))
          cards)))

(defn probability [x]
  (double (or x 0)))

(defn card-risk [analyses card]
  (probability (get-in analyses [card :prob-pending-opponent-can-beat-card])))

(defn risk-adjusted-lead-value [config game analyses card]
  (- (card-score game card)
     (* (:lead-risk-penalty config) (card-risk analyses card))))

(defn risk-adjusted-win-cost [config game analyses card]
  (+ (card-score game card)
     (* (:win-risk-penalty config) (card-risk analyses card))))

(defn safe-cards [config analyses threshold-key cards]
  (filter #(<= (card-risk analyses %) (threshold-key config)) cards))

(defn probability-lead-card [config game analyses cards]
  (let [safe (safe-cards config analyses :lead-risk-tolerance cards)]
    (if (seq safe)
      (lowest-card game safe)
      (first (sort-by #(risk-adjusted-lead-value config game analyses %)
                      >
                      cards)))))

(defn karbosh-caller-lead-card [config game analyses cards]
  (let [trumps (filter #(trump-card? (:trump game) %) cards)]
    (if (seq trumps)
      (first (sort-by #(risk-adjusted-lead-value config game analyses %)
                      >
                      trumps))
      (probability-lead-card config game analyses cards))))

(defn probability-winning-card [config game analyses cards]
  (let [safe (safe-cards config analyses :win-risk-tolerance cards)]
    (if (seq safe)
      (lowest-card game safe)
      (first (sort-by #(risk-adjusted-win-cost config game analyses %)
                      cards)))))

(defn probability-card-action [game player]
  (let [cards (vec (legal-cards game player))
        winner (current-trick-winner game)
        config (context-play-config *play-config* game player)
        analyses (card-analyses game player cards)
        winning-cards (filter #(wins-trick? game player %) cards)
        card (cond
               (empty? cards)
               nil

               (empty? (:current-trick game))
               (if (special-contract-caller? game player)
                 (karbosh-caller-lead-card config game analyses cards)
                 (probability-lead-card config game analyses cards))

               (same-team? game player winner)
               (lowest-card game cards)

               (seq winning-cards)
               (probability-winning-card config game analyses winning-cards)

               :else
               (lowest-card game cards))]
    (when card
      {:type :play-card
       :card card})))

(defn hybrid-card-action [game player]
  (if (special-contract? (game/current-bid game))
    (card-counting-card-action game player)
    (probability-card-action game player)))

(def play-strategies
  {:card-counting card-counting-card-action
   :probability probability-card-action
   :hybrid hybrid-card-action})

(defn resolve-play-strategy [strategy]
  (cond
    (fn? strategy) strategy
    (keyword? strategy) (or (get play-strategies strategy)
                            (throw (ex-info "Unknown play strategy"
                                            {:strategy strategy
                                             :available (keys play-strategies)})))
    :else (throw (ex-info "Invalid play strategy" {:strategy strategy}))))

(defn card-action
  ([game player]
   (card-action game player *play-strategy*))
  ([game player strategy]
   ((resolve-play-strategy strategy) game player)))

(defn action [game player]
  (case (:phase game)
    :bidding (bid-action game player)
    :trump-selection (trump-action game player)
    :karbosh-donation (donate-action game player)
    :karbosh-discard (discard-action game player)
    :trick-playing (card-action game player)
    nil))
