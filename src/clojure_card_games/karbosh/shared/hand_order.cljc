(ns clojure-card-games.karbosh.shared.hand-order
  (:require [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(defn remove-first-card [card hand]
  (let [[before after] (split-with #(not= card %) hand)]
    (if (seq after)
      (vec (concat before (rest after)))
      (vec hand))))

(defn visible-hand [hand pending-card]
  (let [hand (vec (or hand []))]
    (if (and pending-card (some #(= pending-card %) hand))
      (remove-first-card pending-card hand)
      hand)))

(defn reconcile [hand-order hand hand-index]
  (let [hand (vec (or hand []))
        ordered (if (= (:hand-index hand-order) hand-index)
                  (:cards hand-order)
                  [])]
    (loop [remaining hand
           ordered ordered
           kept []]
      (if-let [card (first ordered)]
        (if (some #(= card %) remaining)
          (recur (remove-first-card card remaining)
                 (rest ordered)
                 (conj kept card))
          (recur remaining (rest ordered) kept))
        {:hand-index hand-index
         :cards (vec (concat kept remaining))}))))

(defn index-of-card [cards card]
  (first (keep-indexed (fn [idx c]
                         (when (= c card) idx))
                       cards)))

(defn insert-at [cards index card]
  (let [cards (vec cards)
        index (max 0 (min index (count cards)))]
    (vec (concat (subvec cards 0 index)
                 [card]
                 (subvec cards index)))))

(defn move-card-to [cards card index]
  (let [cards (vec (or cards []))]
    (if (some #(= card %) cards)
      (let [without-card (remove-first-card card cards)]
        (insert-at without-card index card))
      cards)))

(defn valid-index? [cards index]
  (and (integer? index)
       (<= 0 index)
       (< index (count cards))))

(defn move-index-to
  "Move the card at `from-index` next to `target-index`.

  The result includes the moved card's new index so callers can keep tracking
  the same physical position while drag-reordering duplicate logical cards."
  [cards from-index target-index after?]
  (let [cards (vec (or cards []))]
    (if (and (valid-index? cards from-index)
             (valid-index? cards target-index))
      (let [card (nth cards from-index)
            without-card (vec (concat (subvec cards 0 from-index)
                                      (subvec cards (inc from-index))))
            adjusted-target (if (> target-index from-index)
                              (dec target-index)
                              target-index)
            insert-index (+ adjusted-target (if after? 1 0))
            insert-index (max 0 (min insert-index (count without-card)))]
        {:cards (insert-at without-card insert-index card)
         :index insert-index})
      {:cards cards
       :index from-index})))

(defn suit-color [suit]
  (case suit
    (:♥ :♦) :red
    (:♠ :♣) :black
    nil))

(defn card-effective-suit [trump card]
  (if trump
    (rules/effective-suit card trump)
    (second card)))

(defn suit-card-value [trump suit card]
  (if trump
    (rules/card-value card trump suit)
    (let [[rank card-suit] card
          base ({:A 8 :K 7 :Q 6 :J 5 10 4 9 3} rank 0)]
      (if (= suit card-suit) (* base 10) base))))

(defn suit-strength [trump cards suit]
  (->> cards
       (filter #(= suit (card-effective-suit trump %)))
       (map #(suit-card-value trump suit %))
       (reduce +)))

(defn next-suit [ordered-suits prior-color]
  (let [opposite (first (filter #(not= prior-color (suit-color %))
                                ordered-suits))]
    (or opposite (first ordered-suits))))

(defn alternating-suits [ordered-suits]
  (loop [remaining (vec ordered-suits)
         prior-color nil
         result []]
    (if (empty? remaining)
      result
      (let [suit (or (next-suit remaining prior-color)
                     (first remaining))]
        (recur (vec (remove #(= suit %) remaining))
               (suit-color suit)
               (conj result suit))))))

(defn sorted-suits [hand trump]
  (let [hand (vec (or hand []))
        suits (->> hand
                   (map #(card-effective-suit trump %))
                   distinct)]
    (->> suits
         (sort-by #(- (suit-strength trump hand %)))
         alternating-suits)))

(defn known-trump-sorted-hand [hand trump]
  (let [hand (vec (or hand []))
        grouped (group-by #(card-effective-suit trump %) hand)]
    (->> (sorted-suits hand trump)
         (mapcat (fn [suit]
                   (sort-by #(- (suit-card-value trump suit %))
                            (get grouped suit))))
         vec)))

(def suit-index
  (zipmap cards/suits (range)))

(defn remove-cards [hand cards]
  (reduce (fn [remaining card]
            (remove-first-card card remaining))
          (vec hand)
          cards))

(defn potential-trump-block [hand trump]
  (let [block (->> hand
                   (filter #(= trump (rules/effective-suit % trump)))
                   (sort-by #(- (rules/card-value % trump trump)))
                   vec)
        values (mapv #(rules/card-value % trump trump) block)]
    {:trump trump
     :color (suit-color trump)
     :cards block
     :sort-key [(reduce + values)
                (count block)
                values
                (- (get suit-index trump 0))]}))

(defn strongest-potential-block [hand prior-color]
  (let [blocks (->> cards/suits
                    (map #(potential-trump-block hand %))
                    (filter #(seq (:cards %)))
                    (sort-by :sort-key #(compare %2 %1)))
        alternate-color (first (filter #(not= prior-color (:color %))
                                       blocks))]
    (or alternate-color (first blocks))))

(defn potential-trump-sorted-hand [hand]
  (loop [remaining (vec (or hand []))
         prior-color nil
         sorted []]
    (if-let [{:keys [cards color]} (strongest-potential-block remaining prior-color)]
      (recur (remove-cards remaining cards)
             color
             (into sorted cards))
      sorted)))

(defn sorted-hand
  "Sort a hand by strongest effective suit, alternating suit colors where
  possible, and descending card strength within each suit.

  Before trump is known, repeatedly evaluate the remaining hand under every
  possible trump and put the strongest potential trump block next."
  [hand trump]
  (if trump
    (known-trump-sorted-hand hand trump)
    (potential-trump-sorted-hand hand)))
