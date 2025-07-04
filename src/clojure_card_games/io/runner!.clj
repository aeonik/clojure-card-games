(ns clojure-card-games.io.runner!
  (:require [clojure-card-games.state :as state]
            [clojure-card-games.io.tui :as tui]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn read-config []
  (let [f (io/file "config.edn")]
    (if (.exists f)
      (edn/read-string (slurp f))
      {})))

(defn play-game!
  ([] (play-game! nil nil))
  ([seed replay-seq]
   (loop [game (state/init-game seed)
          actions (seq replay-seq)]
     (tui/print-game-state! game (get (read-config) :sort-hands? false))
     (let [{:keys [next-seq] :as raw} (tui/get-player-action! game actions)
           action (dissoc raw :next-seq)]
       (if (= (:type action) :quit)
         (do (println "Thanks for playing!") (System/exit 0))
         (recur (state/apply-event game action)
                next-seq))))))

(defn -main [& _]
  (let [config (read-config)
        sort-hands? (get config :sort-hands? false)]
    (play-game! nil nil)
    ;; Example usage:
    ;; (tui/print-game-state! game sort-hands?)
    ))