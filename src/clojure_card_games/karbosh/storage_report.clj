(ns clojure-card-games.karbosh.storage-report
  (:require [clojure.java.io :as io]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.storage :as storage]))

(defn file-size [file]
  (let [file (io/file file)]
    (cond
      (not (.exists file)) 0
      (.isFile file) (.length file)
      :else (reduce + (map file-size (rest (file-seq file)))))))

(defn room-file-report [room-dir file]
  (let [room-id (storage/room-id-from-file file)]
    (try
      (let [room (storage/read-room room-dir room-id)
            hands (audit/room-hand-count room)]
        {:room-id room-id
         :bytes (.length (io/file file))
         :hands hands
         :games (count (:games room))
         :phase (get-in room [:game :phase])
         :seed (get-in room [:game :initial-seed])
         :zero-hand? (zero? hands)})
      (catch Throwable t
        {:room-id room-id
         :bytes (.length (io/file file))
         :error (.getMessage t)}))))

(defn report
  [{:keys [room-dir audit-dir]
    :or {room-dir "data/karbosh-rooms"
         audit-dir "data/karbosh-audit"}}]
  (let [room-dir (io/file room-dir)
        audit-dir (io/file audit-dir)
        rooms (mapv #(room-file-report room-dir %) (storage/room-files room-dir))
        malformed (filterv :error rooms)
        zero-hand (filterv :zero-hand? rooms)]
    {:room-dir (.getPath room-dir)
     :audit-dir (.getPath audit-dir)
     :room-files (count rooms)
     :room-bytes (file-size room-dir)
     :audit-bytes (file-size audit-dir)
     :malformed-room-files (count malformed)
     :zero-hand-room-files (count zero-hand)
     :largest-rooms (->> rooms
                         (sort-by :bytes >)
                         (take 10)
                         vec)
     :malformed malformed
     :zero-hand zero-hand}))

(defn parse-args [args]
  (loop [opts {}
         args args]
    (if-let [[arg value & more] (seq args)]
      (case arg
        "--room-dir" (recur (assoc opts :room-dir value) more)
        "--audit-dir" (recur (assoc opts :audit-dir value) more)
        (throw (ex-info "Unknown storage report argument"
                        {:argument arg})))
      opts)))

(defn -main [& args]
  (println (pr-str (report (parse-args args)))))
