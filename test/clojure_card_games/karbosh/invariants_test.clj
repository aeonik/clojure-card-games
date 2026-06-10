(ns clojure-card-games.karbosh.invariants-test
  "Property-style guarantees for the deck, the game engine, and the bot.

  These are the safety gates the heuristic AI must never regress on: every
  action the bot proposes is legal in the phase it is proposed, self-play
  always terminates, scores are conserved, and the same seed with the same
  config produces the same game."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(def playable-phases
  #{:bidding :trump-selection :karbosh-discard :karbosh-donation :trick-playing})

(deftest deck-invariants-test
  (testing "the karbosh deck is two copies of each 9-A card"
    (let [deck (cards/deck)]
      (is (= 48 (count deck)))
      (is (= (zipmap (distinct deck) (repeat 2))
             (frequencies deck)))))

  (testing "shuffling preserves the deck multiset"
    (is (= (frequencies (cards/deck))
           (frequencies (cards/shuffle-deck (cards/deck) 7)))))

  (testing "every deal hands out the full deck"
    (let [g (game/init-game 9)]
      (is (= (frequencies (cards/deck))
             (frequencies (apply concat (vals (:initial-hands g)))))))))

(defn- in-hand? [g player card]
  (some #(= card %) (get-in g [:players player :hand])))

(defn- assert-action-legal! [g player event]
  (case (:type event)
    :bid
    (is (rules/legal-bid? (game/current-bid g) event)
        (str "illegal bid " event " over " (game/current-bid g)))

    :trump-selection
    (is (contains? (set cards/suits) (:suit event))
        (str "illegal trump " event))

    (:discard-card :donate-card)
    (is (in-hand? g player (:card event))
        (str "card not in hand " event))

    :play-card
    (is (some #(= (:card event) %) (bot/legal-cards g player))
        (str "illegal play " event " in trick " (:current-trick g)))

    (is false (str "unexpected action type " event))))

(defn- self-play
  "Bot self-play from `seed` for up to `max-hands` hands, asserting that the
  phase is always playable, every action is legal, and play terminates."
  [seed max-hands]
  (let [step-budget (* 80 max-hands)]
    (loop [g (game/init-game seed)
           steps 0]
      (is (<= steps step-budget)
          (str "self-play exceeded step budget on seed " seed))
      (cond
        (or (> steps step-budget)
            (= :game-over (:phase g)))
        g

        (= :hand-complete (:phase g))
        (if (>= (count (:hand-history g)) max-hands)
          g
          (recur (game/apply-event g {:type :new-hand}) (inc steps)))

        :else
        (let [player (:current-player g)
              _ (is (contains? playable-phases (:phase g))
                    (str "unplayable phase " (:phase g) " on seed " seed))
              event (bot/action g player)]
          (is (some? event)
              (str "bot produced no action in phase " (:phase g)
                   " on seed " seed))
          (assert-action-legal! g player event)
          (recur (game/apply-event g (assoc event :player player))
                 (inc steps)))))))

(defn- game-fingerprint [g]
  {:scores (:scores g)
   :hands (count (:hand-history g))
   :points (:points-per-hand g)
   :tricks (:tricks-per-hand g)
   :bids (mapv #(select-keys % [:player :bid-type :value :hand-index])
               (:bids g))})

(deftest bot-self-play-stays-legal-test
  (doseq [seed (range 1 7)]
    (let [g (self-play seed 2)]
      (testing (str "seed " seed " scores are conserved")
        (is (= (:scores g)
               (reduce (partial merge-with +)
                       {1 0 2 0}
                       (:points-per-hand g))))))))

(deftest bot-self-play-full-game-test
  (let [g (self-play 3 60)]
    (is (= :game-over (:phase g)))
    (is (contains? #{1 2} (:winner g)))
    (is (>= (get (:scores g) (:winner g)) game/target-score))))

(deftest bot-self-play-deterministic-test
  (testing "same seed and same config produce the same game"
    (is (= (game-fingerprint (self-play 11 2))
           (game-fingerprint (self-play 11 2))))))
