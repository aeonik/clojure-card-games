(ns clojure-card-games.karbosh.audit
  (:require [clojure.core.async :as async]
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
