(ns clojure-card-games.karbosh.repl
  (:require [nrepl.server :as nrepl]))

(defonce server* (atom nil))

(def loopback-binds
  #{"127.0.0.1" "localhost" "::1"})

(defn safe-repl-bind? [bind]
  (contains? loopback-binds bind))

(defn start! [{:keys [bind port]
               :or {bind "127.0.0.1"
                    port 7888}}]
  (when-not (safe-repl-bind? bind)
    (throw (ex-info "Refusing to start production REPL on non-loopback bind"
                    {:bind bind
                     :port port})))
  (when-not @server*
    (reset! server*
            (nrepl/start-server
             :bind bind
             :port port)))
  (println (str "Karbosh nREPL listening on " bind ":" port))
  {:status :started
   :bind bind
   :port port})

(defn stop! []
  (when-let [server @server*]
    (nrepl/stop-server server)
    (reset! server* nil))
  {:status :stopped})
