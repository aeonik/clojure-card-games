(ns clojure-card-games.karbosh.shared.rules
  "Karbosh rules: a six-player double-deck Bid Euchre variant.

  Cards are logical `[rank suit]` tuples; the double deck means every logical
  card has two physical copies. Generic trick mechanics live in
  `clojure-card-games.trick`; everything Euchre-flavored (bowers, effective
  suit, bids, scoring) lives here."
  (:require [clojure-card-games.trick :as trick]))

(def bid-types #{:pass :bid :karbosh :double-karbosh})
(def special-bid-points 15)

(defn same-color-suit [suit]
  (case suit
    :♥ :♦
    :♦ :♥
    :♠ :♣
    :♣ :♠
    nil))

(defn left-bower? [[rank suit] trump]
  (and (= rank :J) (= suit (same-color-suit trump))))

(defn right-bower? [[rank suit] trump]
  (and (= rank :J) (= suit trump)))

(defn effective-suit [[rank suit :as card] trump]
  (if (and (= rank :J) (left-bower? card trump))
    trump
    suit))

(defn card-value [card trump lead]
  (let [[rank _] card
        suit (effective-suit card trump)
        base ({:A 8 :K 7 :Q 6 :J 5 10 4 9 3} rank 0)]
    (cond
      (right-bower? card trump) 2000
      (left-bower? card trump) 900
      (= suit trump) (* base 100)
      (= suit lead) (* base 10)
      :else base)))

(defn trick-lead [trick trump]
  (some-> trick first :card (effective-suit trump)))

(defn beats? [trump lead challenger incumbent]
  (> (card-value challenger trump lead)
     (card-value incumbent trump lead)))

(defn winning-play
  "Return the winning play in a trick.

  Equal cards can appear because Karbosh uses two copies of each card. Ties are
  intentionally settled by keeping the earlier play."
  [played trump]
  (when (seq played)
    (let [lead (trick-lead played trump)]
      (trick/winning-play played #(card-value % trump lead)))))

(defn resolve-trick [trick trump]
  (:player (winning-play trick trump)))

(defn valid-bid-value? [value]
  (and (number? value) (<= 1 value 8) (= value (long value))))

(defn valid-bid? [{:keys [bid-type value]}]
  (case bid-type
    :pass true
    :bid (valid-bid-value? value)
    :karbosh true
    :double-karbosh true
    false))

(defn bid-rank [{:keys [bid-type value]}]
  (case bid-type
    :pass 0
    :bid value
    :karbosh 9
    :double-karbosh 10
    0))

(defn legal-bid?
  "Return true when `bid` can be made over `current-bid`.

  Passing is always legal. Any non-pass bid must strictly outrank the current
  winning bid; equal or lower bids are ignored by trick-taking rules but should
  not enter game history."
  [current-bid {:keys [bid-type] :as bid}]
  (and (valid-bid? bid)
       (or (= :pass bid-type)
           (> (bid-rank bid) (bid-rank current-bid)))))

(defn winning-bid [bids]
  (->> bids
       (filter #(and (= (:type %) :bid)
                     (not= (:bid-type %) :pass)))
       (sort-by bid-rank >)
       first))

(defn legal-play? [hand trick card trump]
  (let [lead (trick-lead trick trump)
        follows? (= (effective-suit card trump) lead)
        can-follow? (some #(= (effective-suit % trump) lead) hand)]
    (and (some #(= card %) hand)
         (or (empty? trick)
             follows?
             (not can-follow?)))))

(defn distinct-cards [cards]
  (vec (distinct cards)))

(defn legal-cards
  "Return distinct logical cards that can legally be played from `hand`.

  Duplicate physical copies are collapsed because playing either copy has the
  same public meaning and should not create duplicate solver branches."
  [hand trick trump]
  (->> hand
       distinct-cards
       (filter #(legal-play? hand trick % trump))
       vec))

(defn score-hand
  "Score a completed hand.

  Numeric bids score the bidder's tricks when made, or minus the bid value
  when set; the defending team keeps its tricks only when the bid is set.

  Karbosh and double Karbosh both require all 8 tricks and both score the
  same `special-bid-points` (15) made or set — that is the intended house
  rule. Double Karbosh differs only in bid rank (it outranks Karbosh) and in
  being played without partner donations."
  [players bid tricks]
  (let [bidder-team (get-in players [(:player bid) :team])
        other-team (if (= bidder-team 1) 2 1)
        bid-tricks (get tricks bidder-team 0)
        other-tricks (get tricks other-team 0)]
    (case (:bid-type bid)
      :bid
      (if (>= bid-tricks (:value bid))
        {bidder-team bid-tricks other-team 0}
        {bidder-team (- (:value bid)) other-team other-tricks})

      (:karbosh :double-karbosh)
      (if (= 8 bid-tricks)
        {bidder-team special-bid-points other-team 0}
        {bidder-team (- special-bid-points) other-team other-tricks})

      {1 0 2 0})))
