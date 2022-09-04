(ns gui.core
  (:require [clojure.math.combinatorics :as combo]
            [cljfx :as fx]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(fx/on-fx-thread
 (fx/create-component
  {:fx/type :stage
   :showing true
   :title "Cljfx example"
   :width 300
   :height 100
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :alignment :center
                  :children [{:fx/type :label
                              :text "Hello world"}]}}})) 

(def renderer
  (fx/create-renderer))

(defn root [{:keys [showing]}]
  {:fx/type :stage
   :showing showing
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :padding 50
                  :children [{:fx/type :button
                              :text "close"
                              :on-action (fn [_]
                                           (renderer {:fx/type root
                                                      :showing false}))}]}}})

(renderer {:fx/type root
           :showing true})
