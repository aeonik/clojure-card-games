(ns clojure-card-games.sorting
  "Hand‑sorting & naive trump‑evaluation utilities extracted from the old
  `basic.clj`.  Pure functions, so you can call them from the TUI, a bot, or
  unit tests without side‑effects.

  Public API ──────────
  • `(sort-hand-by-trump hand trump)` → sequence ready for display (trump
    first, bowers treated as trump, then strongest side‑suits).
  • `(find-best-trump hand)`          → {:best-trump :♥, :strength 1234,
                                         :sorted-hand <seq>}
  "
  (:require [clojure-card-games.karbosh.shared.rules :as rules]))

;; -----------------------------------------------------------------------
;; 1.  Helper: naive strength metric  -------------------------------------
;; -----------------------------------------------------------------------

(defn- evaluate-hand-strength
  "Add up `rules/card-value` over the hand under a given trump suit.  Very
  rough but good enough for display sorting and 'which trump should I call'
  UI hints."
  [hand trump]
  (reduce + (map #(rules/card-value % trump nil) hand)))

;; -----------------------------------------------------------------------
;; 2.  Pretty sort for a *given* trump  -----------------------------------
;; -----------------------------------------------------------------------

(defn sort-hand-by-trump
  "Sort hand by card value under the given trump suit.
   Returns a seq, *not* a map — meant purely for display.
  "
  [hand trump]
  (sort-by #(rules/card-value % trump nil) > hand))

;; -----------------------------------------------------------------------
;; 3.  Pick the best trump (naive)  ---------------------------------------
;; -----------------------------------------------------------------------

(defn find-best-trump
  "Evaluate all four suits with `evaluate-hand-strength` and return the best
  along with a pre‑sorted hand for that trump.

      (find-best-trump [[:A :♠] [:A :♠] [:K :♥] [:J :♣] …])
      ;;=> {:best-trump :♠, :strength 1420, :sorted-hand (<seq>)}
  "
  [hand]
  (let [suits [:♥ :♠ :♦ :♣]
        scored (for [tr suits]
                 {:best-trump tr
                  :strength   (evaluate-hand-strength hand tr)
                  :sorted-hand (sort-hand-by-trump hand tr)})]
    (apply max-key :strength scored)))

;; ----------------------------  END  ------------------------------------
