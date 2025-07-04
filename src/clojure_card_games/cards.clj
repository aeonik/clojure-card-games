(ns clojure-card-games.cards)

;; ---------------------------------------------------------------------------
;; 1. Data
;; ---------------------------------------------------------------------------

(def ranks [9 10 :J :Q :K :A])
(def suits [:♥ :♠ :♦ :♣])
(def rank->int (zipmap ranks (range)))
(def suit->int (zipmap suits (range)))

(def suit->base {:♠ 0x1F0A0, :♥ 0x1F0B0, :♦ 0x1F0C0, :♣ 0x1F0D0})
(def rank->offset {9 9, 10 10, :J 11, :Q 12, :K 13, :A 1})

(def rank->str {9 "9" 10 "10" :J "J" :Q "Q" :K "K" :A "A"})
(def suit->str {:♥ "♥" :♠ "♠" :♦ "♦" :♣ "♣"})

(def str->rank {"9" 9 "10" 10 "J" :J "Q" :Q "K" :K "A" :A})
(def str->suit {"♥" :♥ "♠" :♠ "♦" :♦ "♣" :♣})

(def char->rank {\a :A \k :K \q :Q \j :J \0 10
                 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})
(def char->suit {\h :♥, \s :♠, \d :♦, \c :♣})

;; ---------------------------------------------------------------------------
;; 2. Pure helpers
;; ---------------------------------------------------------------------------

(defn- ->str [cp] (String. (Character/toChars cp)))

(defn unicode [[rank suit]]
  (let [base   (suit->base suit)
        offset (rank->offset rank)]
    (assert base   (str "Bad suit " suit))
    (assert offset (str "Bad rank " rank))
    (->str (+ base offset))))

(def suit->ansi {:♥ 31, :♦ 31, :♠ 30, :♣ 30})

(defn sort-hand
  "Sort by suit (♥ < ♠ < ♦ < ♣) then rank (9 < 10 < J < Q < K < A)."
  [hand]
  (sort-by (juxt (comp suit->int second)
                 (comp rank->int first))
           hand))