(ns clojure-card-games.karbosh.storage
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-card-games.karbosh.room :as room])
  (:import [java.nio.file Files StandardCopyOption]))

(defn room-file [dir room-id]
  (io/file dir (str room-id ".edn")))

(defn persisted-seat [seat]
  (cond-> seat
    (not (:bot? seat)) (assoc :connected? false)))

(defn offline-room [room]
  (-> room
      (dissoc :connections)
      (assoc :connections {})
      (update :seats #(into {} (map (fn [[player seat]]
                                      [player (persisted-seat seat)])
                                    %)))))

(defn persisted-room [room]
  (assoc (offline-room room) :updated-at (System/currentTimeMillis)))

(defn loaded-room [room]
  (some-> room
          (offline-room)
          (room/ensure-room-metadata)))

(defn read-room [dir room-id]
  (let [file (room-file dir room-id)]
    (when (and (.exists file) (.isFile file))
      (loaded-room (edn/read-string (slurp file))))))

(defn write-room! [dir room]
  (let [file (room-file dir (:id room))
        tmp (io/file dir (str (:id room) ".edn.tmp"))]
    (io/make-parents file)
    (spit tmp (str (pr-str (persisted-room room)) "\n"))
    (Files/move (.toPath tmp)
                (.toPath file)
                (into-array StandardCopyOption
                            [StandardCopyOption/REPLACE_EXISTING
                             StandardCopyOption/ATOMIC_MOVE]))
    file))

(defn delete-room! [dir room-id]
  (let [file (room-file dir room-id)]
    (when (.exists file)
      (.delete file))))

(defn room-exists? [dir room-id]
  (let [file (room-file dir room-id)]
    (and (.exists file) (.isFile file))))

(defn room-files [dir]
  (let [dir (io/file dir)]
    (if (and (.exists dir) (.isDirectory dir))
      (->> (file-seq dir)
           (filter #(.isFile %))
           (filter #(.endsWith (.getName %) ".edn"))
           (sort-by #(.lastModified %))
           vec)
      [])))

(defn room-id-from-file [file]
  (str/replace (.getName (io/file file)) #"\.edn$" ""))
