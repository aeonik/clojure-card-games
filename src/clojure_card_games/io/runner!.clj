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
   (let [config (read-config)
         sort-hands? (:sort-hands? config false)]
     (loop [game (state/init-game seed)
            actions (seq replay-seq)
            move-chars []
            seed-seq [seed]]
       (tui/print-game-state! game sort-hands?)
       (let [{:keys [next-seq input] :as raw} (tui/get-player-action! game actions)
             action (dissoc raw :next-seq :input)
             new-move-chars (if input (concat move-chars (seq input)) move-chars)]
         (if (= (:type action) :quit)
           (do
             (println "Thanks for playing!")
             (println (str "Seed: " (first seed-seq)))
             (println (str "Move sequence: " (apply str new-move-chars)))
             (println (str "Seed sequence: " seed-seq))
             (System/exit 0))
           (let [new-game (state/apply-event game action)
                 ;; If a new hand started, derive a new seed
                 new-seed-seq (if (and (= (:phase game) :hand-complete)
                                       (= (:phase new-game) :bidding))
                                (conj seed-seq (state/derive-seed game))
                                seed-seq)]
             (recur new-game
                    next-seq
                    new-move-chars
                    new-seed-seq))))))))

(defn -main [& _]
  (play-game! nil nil))
