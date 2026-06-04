(ns clojure-card-games.karbosh.runtime)

(defonce handler* (atom nil))

(defonce ws-handlers*
  (atom {:on-message nil
         :on-close nil}))

(defn current-handler [request]
  (if-let [handler @handler*]
    (handler request)
    {:status 503
     :headers {"Content-Type" "text/plain; charset=utf-8"}
     :body "Karbosh handler not loaded"}))

(defn install-handler! [handler]
  (reset! handler* handler)
  :ok)

(defn install-ws-handlers! [handlers]
  (swap! ws-handlers* merge handlers)
  :ok)

(defn dispatch-message [conn-id out session raw]
  (if-let [on-message (:on-message @ws-handlers*)]
    (on-message conn-id out session raw)
    (throw (ex-info "Karbosh websocket message handler not loaded"
                    {:conn-id conn-id}))))

(defn dispatch-close [conn-id in out session]
  (when-let [on-close (:on-close @ws-handlers*)]
    (on-close conn-id in out session)))
