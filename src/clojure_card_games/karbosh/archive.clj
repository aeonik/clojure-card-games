(ns clojure-card-games.karbosh.archive
  (:require [clojure.java.io :as io]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.storage :as storage])
  (:import [java.nio.file Files StandardCopyOption]
           [java.time Instant ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def confirm-token "COMPACT_ARCHIVE")

(defn file-size [file]
  (let [file (io/file file)]
    (cond
      (not (.exists file)) 0
      (.isFile file) (.length file)
      :else (reduce + (map file-size (rest (file-seq file)))))))

(defn timestamp-stamp []
  (.format (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss'Z'")
           (.atZone (Instant/now) ZoneOffset/UTC)))

(defn backup-dir [audit-dir]
  (let [base (io/file (str (.getPath (io/file audit-dir))
                           ".compacted-"
                           (timestamp-stamp)))]
    (loop [n 0]
      (let [candidate (if (zero? n)
                        base
                        (io/file (str (.getPath base) "-" n)))]
        (if (.exists candidate)
          (recur (inc n))
          candidate)))))

(defn move-audit-dir! [audit-dir]
  (let [src (io/file audit-dir)
        dst (backup-dir src)]
    (when (and (.exists src) (.isDirectory src))
      (Files/move (.toPath src)
                  (.toPath dst)
                  (into-array StandardCopyOption []))
      (.mkdirs src)
      (.getPath dst))))

(defn entry-hand-count [entry]
  (audit/room-hand-count {:game (:game entry) :games []}))

(defn played-entry? [entry]
  (and (:game entry)
       (pos? (entry-hand-count entry))))

(defn game-entry-key [entry]
  [(:seed entry) (:started-at entry)])

(defn better-entry? [candidate existing]
  (or (nil? existing)
      (> (entry-hand-count candidate) (entry-hand-count existing))
      (and (= (entry-hand-count candidate) (entry-hand-count existing))
           (> (or (:completed-at candidate) 0)
              (or (:completed-at existing) 0)))))

(defn dedupe-entries [entries]
  (vals
   (reduce (fn [by-game entry]
             (let [k (game-entry-key entry)]
               (if (better-entry? entry (get by-game k))
                 (assoc by-game k entry)
                 by-game)))
           {}
           (filter played-entry? entries))))

(defn reindexed-entries [entries]
  (->> entries
       dedupe-entries
       (sort-by (juxt #(or (:started-at %) 0)
                      #(or (:completed-at %) 0)
                      #(str (:seed %))))
       (map-indexed (fn [idx entry]
                      (assoc entry :game-index idx)))
       vec))

(defn record-entry [record]
  (let [room (:room record)]
    (when-let [game (:game room)]
      (cond-> {:seed (audit/game-seed record)
               :started-at (audit/game-timestamp record)
               :game game}
        (audit/completed-game-record? record)
        (assoc :completed-at (:logged-at record))))))

(defn room-current-entry [room]
  (when-let [game (:game room)]
    (cond-> {:seed (get-in room [:game :initial-seed])
             :started-at (or (:game-started-at room)
                             (:created-at room))
             :game game}
      (= :game-over (:phase game))
      (assoc :completed-at (:updated-at room)))))

(defn room-history-entries [room]
  (map-indexed (fn [idx entry]
                 (assoc entry :game-index (or (:game-index entry) idx)))
               (:games room)))

(defn current-record [audit-file game-records]
  (let [latest (audit/latest-record audit-file)]
    (or (when (audit/played-record? latest) latest)
        (first game-records))))

(defn compact-room
  "Build the compact durable room representation for one legacy audit file."
  [audit-file]
  (let [game-records (audit/game-records-from-candidates
                      (audit/game-candidate-records audit-file))]
    (when-let [record (current-record audit-file game-records)]
      (let [source-room (:room record)
            current-entry (record-entry record)
            current-key (game-entry-key current-entry)
            history (->> (concat (room-history-entries source-room)
                                 (keep record-entry game-records))
                         (remove #(= current-key (game-entry-key %)))
                         reindexed-entries)]
        (-> source-room
            (dissoc :games :connections)
            (assoc :seed (:seed current-entry)
                   :game (:game current-entry)
                   :game-started-at (:started-at current-entry)
                   :game-index (count history)
                   :games history)
            room/ensure-room-metadata)))))

(defn merge-room-history [existing migrated]
  (if-not existing
    migrated
    (let [current-key (some-> existing room-current-entry game-entry-key)
          history (->> (concat (:games existing)
                               (:games migrated)
                               [(room-current-entry migrated)])
                       (remove nil?)
                       (remove #(= current-key (game-entry-key %)))
                       reindexed-entries)]
      (-> existing
          (assoc :game-index (count history)
                 :games history)
          room/ensure-room-metadata))))

(defn compact-file! [audit-file room-dir]
  (when-let [migrated (compact-room audit-file)]
    (let [existing (storage/read-room room-dir (:id migrated))
          compacted (merge-room-history existing migrated)
          file (storage/write-room! room-dir compacted)]
      {:room-id (:id compacted)
       :source-bytes (.length (io/file audit-file))
       :history-games (count (:games compacted))
       :current-seed (get-in compacted [:game :initial-seed])
       :room-file (.getPath file)})))

(defn analyze-file [audit-file]
  (when-let [room (compact-room audit-file)]
    {:room-id (:id room)
     :source-bytes (.length (io/file audit-file))
     :history-games (count (:games room))
     :current-seed (get-in room [:game :initial-seed])}))

(defn compact-archive!
  [{:keys [audit-dir room-dir confirm]
    :or {audit-dir "data/karbosh-audit"
         room-dir "data/karbosh-rooms"}}]
  (let [audit-dir (io/file audit-dir)
        room-dir (io/file room-dir)
        write? (= confirm confirm-token)
        files (audit/room-files audit-dir)
        before-bytes (file-size audit-dir)
        existing-room-bytes (file-size room-dir)
        results (if write?
                  (into [] (keep #(compact-file! % room-dir)) files)
                  (into [] (keep analyze-file) files))
        backup (when write?
                 (move-audit-dir! audit-dir))]
    {:status (if write? :compacted :dry-run)
     :audit-dir (.getPath audit-dir)
     :room-dir (.getPath room-dir)
     :backup-dir backup
     :audit-files (count files)
     :rooms (count results)
     :history-games (reduce + (map :history-games results))
     :audit-bytes-before before-bytes
     :room-bytes-before existing-room-bytes
     :room-bytes-after (file-size room-dir)
     :results results}))

(defn parse-args [args]
  (loop [opts {}
         args args]
    (if-let [[arg value & more] (seq args)]
      (case arg
        "--audit-dir" (recur (assoc opts :audit-dir value) more)
        "--room-dir" (recur (assoc opts :room-dir value) more)
        "--confirm" (recur (assoc opts :confirm value) more)
        (throw (ex-info "Unknown archive compaction argument"
                        {:argument arg})))
      opts)))

(defn -main [& args]
  (println (pr-str (compact-archive! (parse-args args)))))
