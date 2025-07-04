(ns gui.core
  (:require [membrane.ui :as ui]
            [membrane.java2d :as java2d]
            [membrane.component :refer [defui make-app]]
            [clojure-card-games.basic :as logic]))

;; === Config ===
(def font-size 180)
(def cards-per-row 8)

(defn calculate-card-dimensions [font-size]
  (let [test-label (ui/label "A♠" (ui/font nil font-size))
        [label-width label-height] (ui/bounds test-label)]
    [(+ label-width 2)  ; Minimal width padding
     (+ label-height 2)]))  ; Minimal height padding

(defn center-text
  "Centres an element that has a non-zero origin (e.g. text) inside a box
   [box-w box-h]."
  [elem [box-w box-h]]
  (let [[w h]   (ui/bounds  elem)    ;; descent box
        [ox oy] (ui/origin  elem)]   ;; ascent is -oy
    (ui/translate
     (- (/ box-w w 2) ox)           ;; (card-w - w)/2 - ox
     (- (/ box-h h 2) oy)           ;; (card-h - h)/2 - oy
     elem)))

(def card-dimensions (calculate-card-dimensions font-size))
(def card-width (first card-dimensions))
(def card-height (second card-dimensions))
(def card-margin 10)

(defn render-card [[rank suit]]
  (let [fg    (case suit (:♥ :♦) [1 0 0] [0 0 0])
        glyph (ui/with-color fg
                (ui/label (logic/unicode-card suit rank)
                          (ui/font nil font-size)))]
    [(ui/with-style :membrane.ui/style-stroke
       (ui/rectangle card-width card-height))
     (center-text glyph [card-width card-height])]))

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

;; === State ===
(def initial-state
  {:deck (logic/init-deck)})

(defui root [{:keys [deck]}]
  (ui/padding 10 10 (apply ui/vertical-layout
                           [(ui/button "Shuffle"
                                       (fn []
                                         [[:set :deck (logic/init-deck)]]))
                            (ui/spacer 0 20)
                            (into [] (layout-cards deck))])))

(defn -main []
  (println "Starting Karbosh visualizer...")
  (java2d/run (make-app #'root initial-state)))
