(ns gui.simple-gui
  (:require [membrane.ui :as ui]
            [membrane.java2d :as java2d]
            [membrane.component :refer [defui make-app]]
            [clojure-card-games.basic :as logic]))

;; === Config ===
(def card-width 60)
(def card-height 90)
(def card-margin 10)
(def cards-per-row 8)

;; Font configuration
(def card-font
  (ui/font "Segoe UI Symbol" 24))  ; Use a font that supports playing card symbols

;; Unicode playing card symbols
(def card-symbols
  {:spades   {9 "🂩" 10 "🂪" :J "🂫" :Q "🂭" :K "🂮" :A "🂡"}
   :hearts   {9 "🂹" 10 "🂺" :J "🂻" :Q "🂽" :K "🂾" :A "🂱"}
   :diamonds {9 "🃉" 10 "🃊" :J "🃋" :Q "🃍" :K "🃎" :A "🃁"}
   :clubs    {9 "🃙" 10 "🃚" :J "🃛" :Q "🃝" :K "🃞" :A "🃑"}})

(defn rank->char [rank]
  (cond
    (number? rank) (str rank)
    (= rank :A) "A"
    (= rank :K) "K"
    (= rank :Q) "Q"
    (= rank :J) "J"))

(defn karbosh-deck []
  (let [ranks [9 10 :J :Q :K :A]
        suits [:spades :hearts :diamonds :clubs]
        cards (for [suit suits
                    rank ranks]
                [(get-in card-symbols [suit rank])])]
    (vec (concat cards cards))))

;; === Grid Layout ===
(defn render-card [card]
  (let [suit (second (first card))
        color (case suit
                :hearts [1 0 0]    ; red
                :diamonds [1 0 0]  ; red
                :spades [0 0 0]    ; black
                :clubs [0 0 0]     ; black
                [0 0 0])          ; default black
        card-symbol (first (first card))]
    (ui/fixed-bounds [card-width card-height]
      (ui/vertical-layout
       [(ui/with-style :membrane.ui/style-stroke
          (ui/rectangle card-width card-height))
        (ui/with-color color
          (ui/center
           (ui/label card-symbol card-font)
           [card-width card-height]))]))))

(defn layout-grid
  "Creates a grid layout function for arranging items in rows and columns"
  [cols width height margin render]
  (fn [coll]
    (map-indexed
     (fn [i item]
       (let [x (* (mod i cols) (+ width margin))
             y (* (quot i cols) (+ height margin))]
         (ui/translate x y (render item))))
     coll)))

(def layout-cards
  (layout-grid cards-per-row card-width card-height card-margin render-card))

;; === State and UI ===
(def initial-state
  {:deck (karbosh-deck)})

(defui root [{:keys [deck]}]
  (ui/vertical-layout
   [(ui/button "Shuffle"
               (fn []
                 [[:set :deck (shuffle deck)]]))
    (ui/spacer 0 20)
    (into [] (layout-cards deck))]))

(defn -main []
  (println "Starting Karbosh visualizer...")
  (java2d/run (make-app #'root initial-state)))


