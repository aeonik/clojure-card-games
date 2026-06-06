(ns clojure-card-games.karbosh.audit
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

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

(defn latest-record [file]
  (last (read-records file)))

(defn room-records [dir room-id]
  (read-records (room-file dir room-id)))

(defn latest-room-record [dir room-id]
  (latest-record (room-file dir room-id)))

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
       (sort-by :logged-at >)
       vec))

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
