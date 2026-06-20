(ns clojure-card-games.karbosh.bot.cards
  "Card-level helpers shared by the bot's bid, inference, play, and explain
  modules: card scoring, hand bookkeeping, contract context, safety
  predicates, and delegation into the hypergeometric analysis layer.

  Functions here never read policy configuration dynamically; anything
  config-dependent takes the config map explicitly."
  (:require [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

;; ---------------------------------------------------------------------------
;; Card predicates and hand bookkeeping
;; ---------------------------------------------------------------------------

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

(defn trump-card-value [trump card]
  (rules/card-value card trump (rules/effective-suit card trump)))

(defn low-trump? [trump card]
  (and (trump-card? trump card)
       (< (rules/card-value card trump trump)
          (rules/card-value [:Q trump] trump trump))))

(defn weakest-cards [trump n hand]
  (->> hand
       (sort-by #(trump-card-value trump %))
       (take n)
       vec))

(defn strongest-card-for-trump [trump hand]
  (first (sort-by #(trump-card-value trump %) > hand)))

(defn remove-first-card [hand card]
  (game/remove-first card hand))

(defn remove-cards [hand cards]
  (reduce remove-first-card (vec hand) cards))

(defn karbosh-discard-cards [hand trump]
  (weakest-cards trump 2 hand))

;; ---------------------------------------------------------------------------
;; Analysis delegation
;; ---------------------------------------------------------------------------

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

(defn card-analyses [game player cards]
  (let [trump (:trump game)
        unseen (vec (unseen-cards game player))
        population-size (count unseen)
        counts (analysis/effective-suit-counts trump unseen)]
    (into {}
          (mapv
           (fn [card]
             [card
              (analysis/card-defeat-analysis game
                                             player
                                             trump
                                             unseen
                                             counts
                                             population-size
                                             card)])
          cards))))

;; ---------------------------------------------------------------------------
;; Card scoring
;; ---------------------------------------------------------------------------

(defn card-score [game card]
  (let [lead (or (some-> (:current-trick game) first :card
                         (rules/effective-suit (:trump game)))
                 (rules/effective-suit card (:trump game)))]
    (rules/card-value card (:trump game) lead)))

(defn lowest-card [game cards]
  (first (sort-by #(card-score game %) cards)))

(defn highest-card [game cards]
  (first (sort-by #(card-score game %) > cards)))

(defn card-effective-suit [game card]
  (rules/effective-suit card (:trump game)))

(defn potential-card-score [game card]
  (rules/card-value card (:trump game) (card-effective-suit game card)))

(defn probability [x]
  (double (or x 0)))

(defn round-probability [x]
  (/ (Math/round (* 1000.0 (probability x))) 1000.0))

(defn card-risk [analyses card]
  (probability (get-in analyses [card :prob-pending-opponent-can-beat-card])))

(defn high-preservation-trump? [game card]
  (and (trump-card? (:trump game) card)
       (>= (card-score game card)
           (rules/card-value [:K (:trump game)] (:trump game) (:trump game)))))

;; ---------------------------------------------------------------------------
;; Contract and trick context
;; ---------------------------------------------------------------------------

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

(defn contract-caller? [game player]
  (= player (:player (game/current-bid game))))

(defn maker-team? [game player]
  (when-let [bid (game/current-bid game)]
    (= (game/player-team game player)
       (game/player-team game (:player bid)))))

(defn lead-context [game player]
  (let [bid (game/current-bid game)
        maker? (boolean (and bid (maker-team? game player)))]
    {:bid bid
     :trump (:trump game)
     :caller? (= player (:player bid))
     :maker-team? maker?
     :defender? (boolean (and bid (not maker?)))}))

(defn remaining-trick-players-after [game player]
  (let [remaining (- (count (game/trick-players game))
                     (count (:current-trick game))
                     1)]
    (take (max 0 remaining)
          (rest (iterate #(game/next-trick-player game %) player)))))

(defn pending-opponents-after [game player]
  (remove #(same-team? game player %)
          (remaining-trick-players-after game player)))

(defn pending-partners-after [game player]
  (filter #(same-team? game player %)
          (remaining-trick-players-after game player)))

(defn players-with-cards [game players]
  (filter #(pos? (count (get-in game [:players % :hand]))) players))

;; ---------------------------------------------------------------------------
;; Trick safety predicates
;; ---------------------------------------------------------------------------

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

(defn can-be-beaten-in-suit-by? [game unseen-counts card lead]
  (let [trump (:trump game)
        value (rules/card-value card trump lead)]
    (some (fn [[hidden-card n]]
            (and (pos? n)
                 (= lead (rules/effective-suit hidden-card trump))
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

(defn in-suit-control-card? [game unseen-counts card]
  (not (can-be-beaten-in-suit-by? game
                                  unseen-counts
                                  card
                                  (card-effective-suit game card))))

(defn remaining-hand-after [game player card]
  (remove-first-card (get-in game [:players player :hand]) card))

(defn suit-counts [game hand]
  (frequencies (map #(card-effective-suit game %) hand)))

(defn same-suit-control-after-discard? [game player unseen-counts card]
  (let [suit (card-effective-suit game card)
        score (potential-card-score game card)]
    (some #(and (= suit (card-effective-suit game %))
                (>= (potential-card-score game %) score)
                (in-suit-control-card? game unseen-counts %))
          (remaining-hand-after game player card))))
