(ns gui.core
  (:require [membrane.ui :as ui]
            [membrane.java2d :as java2d]
            [membrane.component :refer [defui make-app]]
            [clojure-card-games.basic :as logic]))

;; === Config ===
(def card-width 60)
(def card-height 90)
(def card-margin 10)
(def cards-per-row 8)

(defn render-card [[rank suit]]
  (let [label-color (case suit
                      (:♥ :♦) [1 0 0]  ; red hearts and diamonds
                      (:♠ :♣) [0 0 0]  ; black spades and clubs
                      [0 0 0])         ; default black
        card-background (ui/with-style :membrane.ui/style-stroke
                          (ui/rectangle card-width card-height))
        label (ui/with-color label-color
                (ui/center
                 (ui/label (str rank suit))
                 [card-width card-height]))]
    [card-background label]))

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
