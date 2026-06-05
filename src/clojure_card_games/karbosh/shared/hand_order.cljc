(ns clojure-card-games.karbosh.shared.hand-order)

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
