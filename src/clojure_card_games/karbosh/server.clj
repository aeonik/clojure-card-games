(ns clojure-card-games.karbosh.server
  (:gen-class)
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-card-games.karbosh.admin :as admin]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.shared.game :as game]
            [org.httpkit.server :as http])
  (:import [java.net URLDecoder]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Base64]))

(defonce rooms (atom {}))
(defonce server (atom nil))
(defonce bot-turns (atom {}))
(defonce open-websockets (atom #{}))
(defonce metrics
  (atom {:started-at (System/currentTimeMillis)}))

(def reloadable-namespaces
  '[clojure-card-games.karbosh.shared.cards
    clojure-card-games.karbosh.shared.rules
    clojure-card-games.karbosh.shared.game
    clojure-card-games.karbosh.room
    clojure-card-games.karbosh.bot
    clojure-card-games.karbosh.admin
    clojure-card-games.karbosh.server])

(def bot-action-delay-ms 1300)
(def trick-complete-delay-ms 4850)

(defn parse-port []
  (Long/parseLong (or (System/getenv "KARBOSH_PORT") "8090")))

(defn bind-address []
  (or (System/getenv "KARBOSH_BIND") "127.0.0.1"))

(defn max-message-bytes []
  (Long/parseLong (or (System/getenv "KARBOSH_MAX_MESSAGE_BYTES") "8192")))

(defn max-rooms []
  (Long/parseLong (or (System/getenv "KARBOSH_MAX_ROOMS") "128")))

(defn max-room-connections []
  (Long/parseLong (or (System/getenv "KARBOSH_MAX_ROOM_CONNECTIONS") "24")))

(defn max-websocket-connections []
  (Long/parseLong (or (System/getenv "KARBOSH_MAX_WEBSOCKET_CONNECTIONS") "256")))

(defn static-root []
  (io/file (or (System/getenv "KARBOSH_STATIC_ROOT") "karbosh")))

(defn admin-user []
  (or (System/getenv "KARBOSH_ADMIN_USER") "admin"))

(defn admin-password []
  (not-empty (System/getenv "KARBOSH_ADMIN_PASSWORD")))

(def security-headers
  {"X-Content-Type-Options" "nosniff"
   "Referrer-Policy" "no-referrer"
   "X-Frame-Options" "DENY"
   "Content-Security-Policy" "default-src 'self'; connect-src 'self' ws: wss:; img-src 'self' data:; script-src 'self'; style-src 'self' 'unsafe-inline'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'"})

(defn response
  ([status body] (response status body "text/plain; charset=utf-8"))
  ([status body content-type]
   (response status body content-type {}))
  ([status body content-type headers]
   {:status status
    :headers (merge security-headers headers {"Content-Type" content-type})
    :body body}))

(defn edn-response [body]
  (response 200 (pr-str body) "application/edn; charset=utf-8"))

(defn html-response [body]
  (response 200 body "text/html; charset=utf-8"))

(defn metric! [k]
  (swap! metrics update k (fnil inc 0)))

(defn metric-add! [k n]
  (swap! metrics update k (fnil + 0) n))

(defn record-room-update! [elapsed-ns]
  (swap! metrics
         (fn [metrics]
           (-> metrics
               (update :room-updates (fnil inc 0))
               (update :room-update-total-ns (fnil + 0) elapsed-ns)
               (update :room-update-max-ns (fnil max 0) elapsed-ns)))))

(defn record-error! []
  (metric! :errors))

(defn split-env-list [s]
  (->> (str/split (or s "") #",")
       (map str/trim)
       (remove str/blank?)
       set))

(defn allowed-origins []
  (split-env-list (System/getenv "KARBOSH_ALLOWED_ORIGINS")))

(defn same-host-origins [host]
  #{(str "https://" host)
    (str "http://" host)})

(defn origin-allowed? [request]
  (let [origin (get-in request [:headers "origin"])
        host (get-in request [:headers "host"])
        configured (allowed-origins)]
    (or (str/blank? origin)
        (contains? configured origin)
        (and (empty? configured)
             host
             (contains? (same-host-origins host) origin)))))

(defn utf8-bytes [s]
  (.getBytes (str s) StandardCharsets/UTF_8))

(defn secure-eq? [a b]
  (and a b
       (MessageDigest/isEqual (utf8-bytes a) (utf8-bytes b))))

(defn basic-credentials [request]
  (when-let [authorization (get-in request [:headers "authorization"])]
    (when (str/starts-with? authorization "Basic ")
      (try
        (let [encoded (subs authorization (count "Basic "))
              decoded (String. (.decode (Base64/getDecoder) encoded)
                               StandardCharsets/UTF_8)]
          (str/split decoded #":" 2))
        (catch IllegalArgumentException _
          nil)))))

(defn admin-authorized? [request]
  (when-let [expected-password (admin-password)]
    (let [[user password] (basic-credentials request)]
      (and (secure-eq? (admin-user) user)
           (secure-eq? expected-password password)))))

(defn admin-disabled-response []
  (response 503 "Karbosh admin is disabled; set KARBOSH_ADMIN_PASSWORD."))

(defn admin-unauthorized-response []
  {:status 401
   :headers (merge security-headers
                   {"Content-Type" "text/plain; charset=utf-8"
                    "WWW-Authenticate" "Basic realm=\"Karbosh Admin\""})
   :body "Authentication required"})

(defn decode-query-value [s]
  (URLDecoder/decode (or s "") "UTF-8"))

(defn query-params [query-string]
  (into {}
        (keep (fn [part]
                (when-not (str/blank? part)
                  (let [[k v] (str/split part #"=" 2)]
                    [(keyword (decode-query-value k))
                     (decode-query-value v)]))))
        (str/split (or query-string "") #"&")))

(defn selected-admin-room-id [request]
  (some-> (query-params (:query-string request))
          :room
          str/upper-case
          str/trim
          not-empty))

(defn normalize-room-id [room-id]
  (some-> room-id str/upper-case str/trim not-empty))

(defn admin-dashboard-response [request]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-unauthorized-response)

    :else
    (html-response
     (admin/render-dashboard {:rooms @rooms
                              :selected-room-id (selected-admin-room-id request)
                              :metrics @metrics
                              :pending-bot-count (count @bot-turns)
                              :open-websocket-count (count @open-websockets)
                              :limits {:max-rooms (max-rooms)
                                       :max-room-connections (max-room-connections)
                                       :max-websocket-connections (max-websocket-connections)
                                       :max-message-bytes (max-message-bytes)}
                              :started-at (:started-at @metrics)}))))

(defn reload-karbosh-namespaces! []
  (let [started (System/nanoTime)]
    (doseq [namespace reloadable-namespaces]
      (require namespace :reload))
    (let [elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)
          result {:ok true
                  :reloaded reloadable-namespaces
                  :elapsed-ms elapsed-ms
                  :rooms (count @rooms)
                  :open-websockets (count @open-websockets)}]
      (swap! metrics assoc
             :last-reload-at (System/currentTimeMillis)
             :last-reload-ms elapsed-ms)
      (metric! :reloads)
      result)))

(defn admin-reload-response [request]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-unauthorized-response)

    (not (origin-allowed? request))
    (response 403 "Forbidden")

    :else
    (try
      (edn-response (reload-karbosh-namespaces!))
      (catch Throwable t
        (record-error!)
        (response 500
                  (str "Reload failed: " (.getMessage t))
                  "text/plain; charset=utf-8")))))

(def content-types
  {"css" "text/css; charset=utf-8"
   "html" "text/html; charset=utf-8"
   "js" "application/javascript; charset=utf-8"
   "png" "image/png"
   "svg" "image/svg+xml"})

(defn extension [path]
  (some-> (re-find #"\.([A-Za-z0-9]+)$" path) second str/lower-case))

(defn static-file [uri]
  (let [rel (cond
              (= uri "/karbosh") "index.html"
              (= uri "/karbosh/") "index.html"
              (str/starts-with? uri "/karbosh/") (subs uri (count "/karbosh/"))
              :else nil)]
    (when (and rel (not (str/includes? rel "..")))
      (let [file (io/file (static-root) rel)]
        (when (and (.exists file) (.isFile file))
          file)))))

(defn static-response [uri]
  (when-let [file (static-file uri)]
    {:status 200
     :headers (merge security-headers
                     {"Cache-Control" "no-store, no-cache, must-revalidate"
                      "Content-Type" (get content-types (extension (.getName file))
                                          "application/octet-stream")})
     :body file}))

(defn read-message [s]
  (edn/read-string {:readers {} :default (fn [tag value] [tag value])} s))

(defn room-preview [room]
  (let [state (:game room)]
    {:room-id (:id room)
     :phase (:phase state)
     :players (mapv (fn [player]
                      (let [seat (get-in room [:seats player])]
                        {:id player
                         :team (get-in state [:players player :team])
                         :name (:name seat)
                         :bot? (true? (:bot? seat))
                         :connected? (true? (:connected? seat))
                         :open? (nil? seat)
                         :joinable? (or (nil? seat)
                                        (true? (:bot? seat)))}))
                    game/players)}))

(defn room-preview-id [uri]
  (when (str/starts-with? uri "/karbosh/api/room/")
    (normalize-room-id
     (decode-query-value (subs uri (count "/karbosh/api/room/"))))))

(defn room-preview-response [room-id]
  (if-let [room (get @rooms room-id)]
    (edn-response (assoc (room-preview room) :ok true))
    (response 404
              (pr-str {:ok false
                       :room-id room-id
                       :message "Room not found"})
              "application/edn; charset=utf-8")))

(defn send-edn! [out message]
  (metric! :outgoing-messages)
  (async/put! out message))

(defn broadcast-room! [room]
  (let [views (vec (room/connection-views room))]
    (metric! :broadcasts)
    (metric-add! :state-messages (count views))
    (doseq [{:keys [out message]} views]
      (send-edn! out message))))

(declare schedule-bot-turn!)

(defn bot-turn-token [room]
  (when-let [{:keys [player event]} (room/bot-turn room)]
    [player
     (get-in room [:game :phase])
     (count (get-in room [:game :history]))
     (count (get-in room [:game :current-trick]))
     event]))

(defn completed-trick-state? [room]
  (let [state (:game room)
        event (peek (:history state))
        trick (peek (:completed-tricks state))]
    (and (= :play-card (:type event))
         (empty? (:current-trick state))
         (= (count trick) (count (game/trick-players state)))
         (some #(and (= (:player event) (:player %))
                     (= (:card event) (:card %)))
               trick))))

(defn bot-turn-delay-ms [room]
  (if (completed-trick-state? room)
    trick-complete-delay-ms
    bot-action-delay-ms))

(defn publish-room! [room-id room]
  (broadcast-room! room)
  (schedule-bot-turn! room-id room)
  room)

(defn update-room! [room-id f & args]
  (let [started (System/nanoTime)
        new-room (when room-id
                   (get (swap! rooms
                               (fn [rooms]
                                 (if (contains? rooms room-id)
                                   (if-let [room (get rooms room-id)]
                                     (if-let [room' (apply f room args)]
                                       (assoc rooms room-id room')
                                       (dissoc rooms room-id))
                                     (dissoc rooms room-id))
                                   rooms)))
                        room-id))]
    (record-room-update! (- (System/nanoTime) started))
    (when new-room
      (publish-room! room-id new-room))
    new-room))

(defn schedule-bot-turn! [room-id room]
  (when-let [token (bot-turn-token room)]
    (when-not (= token (get @bot-turns room-id))
      (swap! bot-turns assoc room-id token)
      (async/thread
        (Thread/sleep (bot-turn-delay-ms room))
        (let [new-room (get (swap! rooms
                                   (fn [rooms]
                                     (if (contains? rooms room-id)
                                       (if-let [room (get rooms room-id)]
                                         (assoc rooms room-id
                                                (if (= token (bot-turn-token room))
                                                  (room/advance-bot room)
                                                  room))
                                         (dissoc rooms room-id))
                                       rooms)))
                            room-id)]
          (when (= token (get @bot-turns room-id))
            (swap! bot-turns dissoc room-id))
          (when new-room
            (publish-room! room-id new-room)))))))

(defn unique-room-id []
  (loop [id (room/random-room-id)]
    (if (contains? @rooms id)
      (recur (room/random-room-id))
      id)))

(defn room-limit-reached? []
  (>= (count @rooms) (max-rooms)))

(defn room-connection-limit-reached? [room conn-id]
  (and (not (contains? (:connections room) conn-id))
       (>= (count (:connections room)) (max-room-connections))))

(defn websocket-limit-reached? []
  (>= (count @open-websockets) (max-websocket-connections)))

(defn create-room! [conn-id out {:keys [name seed]}]
  (metric! :room-creates)
  (if (room-limit-reached?)
    (do
      (record-error!)
      (send-edn! out {:op :error :message "Room limit reached"})
      nil)
    (let [room-id (unique-room-id)
          seed (or seed (System/currentTimeMillis))
          room (-> (room/new-room room-id seed)
                   (room/join-room {:conn-id conn-id
                                    :out out
                                    :name name}))]
      (swap! rooms assoc room-id room)
      (publish-room! room-id room)
      room-id)))

(defn join-room! [conn-id out {:keys [room-id name player]}]
  (metric! :room-joins)
  (let [room-id (some-> room-id str/upper-case str/trim)
        player (when player (keyword player))]
    (cond
      (not (contains? @rooms room-id))
      (send-edn! out {:op :error :message "Room not found"})

      (room-connection-limit-reached? (get @rooms room-id) conn-id)
      (do
        (record-error!)
        (send-edn! out {:op :error :message "Room connection limit reached"}))

      :else
      (try
        (update-room! room-id room/join-room
                      {:conn-id conn-id
                       :out out
                       :name name
                       :player player})
      (catch Exception e
        (record-error!)
        (send-edn! out {:op :error :message (.getMessage e)}))))))

(defn fill-bots! [room-id out]
  (metric! :fill-bots)
  (cond
    (not room-id)
    (send-edn! out {:op :error :message "Join a room first"})

    (not (contains? @rooms room-id))
    (send-edn! out {:op :error :message "Room not found"})

    :else
    (update-room! room-id room/fill-bots)))

(defn handle-action! [conn-id room-id out {:keys [event]}]
  (metric! :player-actions)
  (if-not (contains? @rooms room-id)
    (send-edn! out {:op :error :message "Room not found"})
    (try
      (update-room! room-id room/apply-player-event conn-id event)
      (catch Exception e
        (record-error!)
        (send-edn! out {:op :error :message (.getMessage e)})))))

(defn auto-play! [conn-id room-id out]
  (metric! :auto-plays)
  (if-not (contains? @rooms room-id)
    (send-edn! out {:op :error :message "Room not found"})
    (try
      (update-room! room-id room/auto-play-player conn-id)
      (catch Exception e
        (record-error!)
        (send-edn! out {:op :error :message (.getMessage e)})))))

(defn disconnect! [conn-id room-id]
  (when room-id
    (update-room! room-id room/remove-connection conn-id)))

(defn handle-client-message! [conn-id out session message]
  (case (:op message)
    :create-room
    (when-let [room-id (create-room! conn-id out message)]
      (reset! session {:room-id room-id}))

    :join-room
    (do
      (join-room! conn-id out message)
      (reset! session {:room-id (some-> (:room-id message) str/upper-case str/trim)}))

    :action
    (handle-action! conn-id (:room-id @session) out message)

    :auto-play
    (auto-play! conn-id (:room-id @session) out)

    :fill-bots
    (fill-bots! (:room-id @session) out)

    :ping
    (send-edn! out {:op :pong})

    (send-edn! out {:op :error :message "Unknown message"})))

(defn handle-raw-message! [conn-id out session raw]
  (let [message-size (alength (utf8-bytes raw))]
    (metric! :incoming-messages)
    (metric-add! :incoming-bytes message-size)
    (if (> message-size (max-message-bytes))
      (do
        (record-error!)
        (send-edn! out {:op :error :message "Message is too large"}))
      (try
        (handle-client-message! conn-id out session (read-message raw))
        (catch Exception e
          (record-error!)
          (send-edn! out {:op :error :message (.getMessage e)}))))))

(defn websocket-handler [request]
  (if (websocket-limit-reached?)
    (do
      (record-error!)
      (response 503 "Websocket connection limit reached"))
    (let [conn-id (random-uuid)
          in (async/chan 32)
          out (async/chan 32)
          session (atom nil)]
      (swap! open-websockets conj conn-id)
      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (http/with-channel request ws
        (http/on-receive ws
                         (fn [raw]
                           (when-not (async/offer! in raw)
                             (record-error!)
                             (send-edn! out {:op :error
                                             :message "Message queue is full"}))))
        (http/on-close ws (fn [_]
                            (swap! open-websockets disj conn-id)
                            (disconnect! conn-id (:room-id @session))
                            (async/close! in)
                            (async/close! out)))
        (async/thread
          (loop []
            (when-let [message (async/<!! out)]
              (http/send! ws (pr-str message))
              (recur))))
        (async/thread
          (loop []
            (when-let [raw (async/<!! in)]
              (handle-raw-message! conn-id out session raw)
              (recur))))))))

(defn admin-path? [uri]
  (or (= uri "/karbosh/admin")
      (= uri "/karbosh/admin/")))

(defn admin-reload-path? [uri]
  (= uri "/karbosh/admin/reload"))

(defn handler [{:keys [uri request-method] :as request}]
  (let [room-preview-id (room-preview-id uri)]
    (cond
      (and (= request-method :get) (= uri "/karbosh/ws"))
      (if (origin-allowed? request)
        (websocket-handler request)
        (response 403 "Forbidden"))

      (and (= request-method :get) (admin-path? uri))
      (admin-dashboard-response request)

      (and (= request-method :post) (admin-reload-path? uri))
      (admin-reload-response request)

      (and (= request-method :get) (= uri "/karbosh/api/health"))
      (edn-response {:ok true :rooms (count @rooms)})

      (and (= request-method :get) room-preview-id)
      (room-preview-response room-preview-id)

      (and (= request-method :get) (str/starts-with? uri "/karbosh"))
      (or (static-response uri) (response 404 "Not found"))

      :else
      (response 404 "Not found"))))

(defn start! []
  (let [port (parse-port)
        ip (bind-address)]
    (reset! server (http/run-server #'handler {:ip ip :port port}))
    (println (str "Karbosh server listening on " ip ":" port))))

(defn stop! []
  (when-let [stop @server]
    (stop)
    (reset! server nil)))

(defn -main [& _]
  (start!)
  @(promise))
