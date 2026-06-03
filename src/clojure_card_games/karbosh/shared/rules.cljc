(ns clojure-card-games.karbosh.shared.rules)

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

(defn resolve-trick [trick trump]
  (let [lead (some-> trick first :card (effective-suit trump))]
    (->> trick
         (sort-by #(card-value (:card %) trump lead) >)
         first
         :player)))

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

(defn winning-bid [bids]
  (->> bids
       (filter #(and (= (:type %) :bid)
                     (not= (:bid-type %) :pass)))
       (sort-by bid-rank >)
       first))

(defn legal-play? [hand trick card trump]
  (let [lead (some-> trick first :card (effective-suit trump))
        follows? (= (effective-suit card trump) lead)
        can-follow? (some #(= (effective-suit % trump) lead) hand)]
    (and (some #(= card %) hand)
         (or (empty? trick)
             follows?
             (not can-follow?)))))

(defn score-hand [players bid tricks]
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
