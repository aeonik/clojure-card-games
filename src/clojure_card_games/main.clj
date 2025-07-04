(ns clojure-card-games.main
  (:gen-class)
  (:require [clojure-card-games.io.runner! :as runner!]))

(defn -main [& args]
  (let [seed (when (seq args)
               (try (Long/parseLong (first args)) (catch Exception _ nil)))
        replay-seq (when (> (count args) 1)
                     (second args))]
    (runner!/play-game! seed replay-seq)))