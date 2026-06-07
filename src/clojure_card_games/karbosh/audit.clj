(ns clojure-card-games.karbosh.audit
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io RandomAccessFile]
           [java.nio.charset StandardCharsets]))

(def schema-version :karbosh.audit/v1)

(defonce writer* (atom nil))

(defn sanitize-room [room]
  (-> room
      (dissoc :connections)
      (update :seats #(into {} %))))

(defn room-record
  ([event-type room]
   (room-record event-type room (System/currentTimeMillis)))
  ([event-type room now]
   {:schema schema-version
    :type event-type
    :logged-at now
    :room-id (:id room)
    :room (sanitize-room room)}))

(defn room-file [dir room-id]
  (io/file dir (str room-id ".edn")))

(defn append-record! [dir record]
  (let [file (room-file dir (:room-id record))]
    (io/make-parents file)
    (spit file (str (pr-str record) "\n") :append true)
    file))

(defn read-records
  "Read valid EDN records from an audit file, preserving file order."
  [file]
  (when (and file (.exists (io/file file)) (.isFile (io/file file)))
    (with-open [reader (io/reader file)]
      (->> (line-seq reader)
           (keep (fn [line]
                   (try
                     (edn/read-string line)
                     (catch Throwable _
                       nil))))
           doall))))

(defn last-line-bounds [^RandomAccessFile raf]
  (let [length (.length raf)]
    (when (pos? length)
      (loop [pos (dec length)
             end length
             skipping-trailing? true]
        (if (neg? pos)
          [0 end]
          (do
            (.seek raf pos)
            (let [b (.read raf)
                  newline? (or (= b 10) (= b 13))]
              (cond
                (and newline? skipping-trailing?)
                (recur (dec pos) pos true)

                newline?
                [(inc pos) end]

                :else
                (recur (dec pos) end false)))))))))

(defn last-line [file]
  (let [file (io/file file)]
    (when (and (.exists file) (.isFile file))
      (with-open [raf (RandomAccessFile. file "r")]
        (when-let [[start end] (last-line-bounds raf)]
          (let [size (- end start)]
            (when (pos? size)
              (let [bytes (byte-array size)]
                (.seek raf start)
                (.readFully raf bytes)
                (not-empty
                 (str/trim
                  (String. bytes StandardCharsets/UTF_8)))))))))))

(defn latest-record [file]
  (when-let [line (last-line file)]
    (try
      (edn/read-string line)
      (catch Throwable _
        nil))))

(defn room-hand-count [room]
  (count (get-in room [:game :hand-history])))

(defn played-room? [room]
  (pos? (room-hand-count room)))

(defn played-record? [record]
  (played-room? (:room record)))

(defn room-records [dir room-id]
  (read-records (room-file dir room-id)))

(defn latest-room-record [dir room-id]
  (let [record (latest-record (room-file dir room-id))]
    (when (played-record? record)
      record)))

(defn room-files [dir]
  (let [dir (io/file dir)]
    (if (and (.exists dir) (.isDirectory dir))
      (->> (file-seq dir)
           (filter #(.isFile %))
           (filter #(.endsWith (.getName %) ".edn"))
           (sort-by #(.lastModified %))
           vec)
      [])))

(defn latest-room-records [dir]
  (->> (room-files dir)
       (keep latest-record)
       (filter played-record?)
       (sort-by :logged-at >)
       vec))

(defn all-room-records [dir]
  (->> (room-files dir)
       (mapcat read-records)
       (sort-by :logged-at >)
       vec))

(defn read-record-line [line]
  (try
    (edn/read-string line)
    (catch Throwable _
      nil)))

(defn game-over-line? [line]
  (str/includes? line ":phase :game-over"))

(defn line-long [pattern line]
  (when-let [[_ n] (re-find pattern line)]
    (try
      (Long/parseLong n)
      (catch NumberFormatException _
        nil))))

(defn line-game-seed [line]
  (line-long #":initial-seed (-?\d+)" line))

(defn line-logged-at [line]
  (line-long #":logged-at (-?\d+)" line))

(defn line-game-timestamp [line]
  (or (line-long #":game-started-at (-?\d+)" line)
      (line-long #":created-at (-?\d+)" line)
      (line-logged-at line)))

(defn game-line-key [room-id line]
  [room-id (line-game-seed line) (line-game-timestamp line)])

(defn game-seed [record]
  (get-in record [:room :game :initial-seed]))

(defn game-timestamp [record]
  (or (get-in record [:room :game-started-at])
      (get-in record [:room :created-at])
      (:logged-at record)))

(defn game-key [record]
  [(:room-id record) (game-seed record) (game-timestamp record)])

(defn completed-game-record? [record]
  (= :game-over (get-in record [:room :game :phase])))

(defn newer-record? [a b]
  (> (or (:logged-at a) 0)
     (or (:logged-at b) 0)))

(defn preferred-game-record [a b]
  (cond
    (nil? a) b
    (nil? b) a
    (and (completed-game-record? b)
         (not (completed-game-record? a))) b
    (and (completed-game-record? a)
         (not (completed-game-record? b))) a
    (newer-record? b a) b
    :else a))

(defn game-candidate-records [file]
  (let [file (io/file file)
        room-id (str/replace (.getName file) #"\.edn$" "")
        latest-record (latest-record file)
        latest (when (played-record? latest-record) latest-record)
        game-over-records (when (and (.exists file) (.isFile file))
                            (with-open [reader (io/reader file)]
                              (->> (line-seq reader)
                                   (filter game-over-line?)
                                   (reduce (fn [lines line]
                                             (let [k (game-line-key room-id line)
                                                   logged-at (or (line-logged-at line) 0)]
                                               (update lines k
                                                       (fn [existing]
                                                         (if (> logged-at
                                                                (or (:logged-at existing) 0))
                                                           {:logged-at logged-at
                                                            :line line}
                                                           existing)))))
                                           {})
                                   vals
                                   (map :line)
                                   (keep read-record-line)
                                   (filter played-record?)
                                   doall)))]
    (cond-> (vec game-over-records)
      latest (conj latest))))

(defn game-records-from-candidates [records]
  (->> records
       (filter game-seed)
       (filter played-record?)
       (reduce (fn [games record]
                 (update games (game-key record) preferred-game-record record))
               {})
       vals
       (sort-by :logged-at >)
       vec))

(defn game-history-records [dir]
  (->> (room-files dir)
       (mapcat game-candidate-records)
       game-records-from-candidates))

(defn room-game-record
  ([dir room-id seed]
   (room-game-record dir room-id seed nil))
  ([dir room-id seed timestamp]
   (->> (game-candidate-records (room-file dir room-id))
        (filter #(and (= (str seed) (str (game-seed %)))
                      (or (nil? timestamp)
                          (= (str timestamp) (str (game-timestamp %))))))
        game-records-from-candidates
        first)))

(defn write-records! [file records]
  (if (seq records)
    (with-open [writer (io/writer file)]
      (doseq [record records]
        (.write writer (pr-str record))
        (.write writer "\n"))
      file)
    (do
      (when (.exists (io/file file))
        (.delete (io/file file)))
      nil)))

(defn prune-room-records!
  "Remove records that do not satisfy `keep?` from one room audit file."
  ([dir room-id]
   (prune-room-records! dir room-id played-record?))
  ([dir room-id keep?]
   (let [file (room-file dir room-id)
         records (vec (or (read-records file) []))
         kept (vec (filter keep? records))]
     (when (not= (count records) (count kept))
       (write-records! file kept))
     {:room-id room-id
      :before (count records)
      :after (count kept)
      :removed (- (count records) (count kept))})))

(defn prune-zero-hand-records! [dir]
  (let [results (mapv #(prune-room-records! dir
                                            (str/replace (.getName %) #"\.edn$" ""))
                      (room-files dir))]
    {:rooms (count results)
     :removed (reduce + (map :removed results))
     :results results}))

(defn start!
  [{:keys [enabled? dir buffer-size]
    :or {enabled? true
         dir "data/karbosh-audit"
         buffer-size 1024}}]
  (if-not enabled?
    (do
      (when-let [{:keys [ch done]} @writer*]
        (async/close! ch)
        (async/<!! done)
        (reset! writer* nil))
      {:status :disabled})
    (or @writer*
        (let [ch (async/chan buffer-size)
              dir (io/file dir)
              done (async/thread
                     (loop []
                       (when-let [record (async/<!! ch)]
                         (try
                           (append-record! dir record)
                           (catch Throwable t
                             (.println System/err
                                       (str "Karbosh audit write failed: "
                                            (.getMessage t)))))
                         (recur))))]
          (reset! writer* {:status :started
                           :dir (.getPath dir)
                           :ch ch
                           :done done})))))

(defn stop! []
  (when-let [{:keys [ch done]} @writer*]
    (async/close! ch)
    (async/<!! done)
    (reset! writer* nil))
  {:status :stopped})

(defn record-room! [event-type room]
  (when-let [{:keys [ch]} @writer*]
    (async/offer! ch (room-record event-type room))))
