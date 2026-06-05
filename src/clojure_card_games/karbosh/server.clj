(ns clojure-card-games.karbosh.server
  (:gen-class)
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-card-games.karbosh.admin :as admin]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.runtime :as runtime]
            [clojure-card-games.karbosh.shared.game :as game]
            [org.httpkit.server :as http])
  (:import [java.net URI URLDecoder]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Base64]))

(defonce rooms (atom {}))
(defonce server (atom nil))
(defonce bot-turns (atom {}))
(defonce open-websockets (atom #{}))
(defonce room-sweeper (atom nil))
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

(declare broadcast-room!)

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

(defn idle-room-ms []
  (Long/parseLong (or (System/getenv "KARBOSH_IDLE_ROOM_MS") "14400000")))

(defn idle-room-sweep-ms []
  (Long/parseLong (or (System/getenv "KARBOSH_IDLE_ROOM_SWEEP_MS") "60000")))

(defn static-root []
  (io/file (or (System/getenv "KARBOSH_STATIC_ROOT") "karbosh")))

(defn nrepl-enabled? []
  (= "true" (str/lower-case (or (System/getenv "KARBOSH_NREPL_ENABLED") ""))))

(defn nrepl-bind []
  (or (System/getenv "KARBOSH_NREPL_BIND") "127.0.0.1"))

(defn nrepl-port []
  (Long/parseLong (or (System/getenv "KARBOSH_NREPL_PORT") "7888")))

(defn admin-user []
  (or (System/getenv "KARBOSH_ADMIN_USER") "admin"))

(defn admin-password []
  (not-empty (System/getenv "KARBOSH_ADMIN_PASSWORD")))

(def security-headers
  {"X-Content-Type-Options" "nosniff"
   "Referrer-Policy" "no-referrer"
   "X-Frame-Options" "DENY"
   "Content-Security-Policy" "default-src 'self'; connect-src 'self' ws: wss:; img-src 'self' data:; media-src 'self' data:; script-src 'self'; style-src 'self' 'unsafe-inline'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'"})

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

(defn redirect-response [location]
  {:status 303
   :headers (merge security-headers
                   {"Location" location
                    "Content-Type" "text/plain; charset=utf-8"})
   :body "See other"})

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

(defn default-port? [scheme port]
  (or (and (= scheme "http") (= port 80))
      (and (= scheme "https") (= port 443))))

(defn canonical-origin [origin]
  (when-not (str/blank? origin)
    (try
      (let [uri (URI. origin)
            scheme (some-> (.getScheme uri) str/lower-case)
            host (some-> (.getHost uri) str/lower-case)
            port (.getPort uri)]
        (when (and (#{"http" "https"} scheme) host)
          (str scheme "://" host
               (when (and (not= -1 port)
                          (not (default-port? scheme port)))
                 (str ":" port)))))
      (catch Exception _
        nil))))

(defn allowed-origins []
  (into #{}
        (keep canonical-origin)
        (split-env-list (System/getenv "KARBOSH_ALLOWED_ORIGINS"))))

(defn same-host-origins [host]
  (let [host (str/trim (or host ""))]
    (into #{}
          (keep canonical-origin)
          [(str "https://" host)
           (str "http://" host)])))

(defn origin-allowed? [request]
  (let [raw-origin (get-in request [:headers "origin"])
        origin (canonical-origin raw-origin)
        host (or (get-in request [:headers "x-forwarded-host"])
                 (get-in request [:headers "host"]))
        configured (allowed-origins)]
    (or (str/blank? raw-origin)
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

(defn admin-delete-room-id [uri]
  (when (str/starts-with? uri "/karbosh/admin/rooms/")
    (normalize-room-id
     (decode-query-value (subs uri (count "/karbosh/admin/rooms/"))))))

(declare start-room-sweeper! install-runtime-handlers!)

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
                                       :max-message-bytes (max-message-bytes)
                                       :idle-room-ms (idle-room-ms)}
                              :started-at (:started-at @metrics)}))))

(defn admin-dashboard-main-html [request]
  (admin/render-dashboard-main-html
   {:rooms @rooms
    :selected-room-id (selected-admin-room-id request)
    :metrics @metrics
    :pending-bot-count (count @bot-turns)
    :open-websocket-count (count @open-websockets)
    :limits {:max-rooms (max-rooms)
             :max-room-connections (max-room-connections)
             :max-websocket-connections (max-websocket-connections)
             :max-message-bytes (max-message-bytes)
             :idle-room-ms (idle-room-ms)}
    :started-at (:started-at @metrics)}))

(defn admin-stream-enabled? [request]
  (= "admin" (str/lower-case (or (some-> (query-params (:query-string request))
                                         :mode
                                         name)
                                 ""))))

(defn admin-stream-response [request]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-unauthorized-response)

    (not (origin-allowed? request))
    (response 403 "Forbidden")

    :else
    (let [stop (atom false)]
      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (http/with-channel request ws
        (http/on-close ws (fn [_] (reset! stop true)))
        (http/send! ws (admin-dashboard-main-html request))
        (async/thread
          (while (not @stop)
            (Thread/sleep 3000)
            (when-not @stop
              (try
                (when-not (http/send! ws (admin-dashboard-main-html request))
                  (reset! stop true))
                (catch Throwable _
                  (reset! stop true))))))))))

(defn refresh-room-view-state! []
  (let [rooms' (swap! rooms
                      (fn [rooms]
                        (reduce-kv (fn [rooms room-id room]
                                     (assoc rooms room-id
                                            (room/ensure-room-metadata room)))
                                   {}
                                   rooms)))]
    (doseq [room (vals rooms')
            :when (seq (:connections room))]
      (broadcast-room! room))
    rooms'))

(defn reload-karbosh-namespaces! []
  (let [started (System/nanoTime)]
    (doseq [namespace reloadable-namespaces]
      (require namespace :reload))
    (start-room-sweeper!)
    (install-runtime-handlers!)
    (refresh-room-view-state!)
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
     :public? (true? (:public? room))
     :phase (:phase state)
     :players (mapv (fn [player]
                      (let [seat (get-in room [:seats player])]
                        {:id player
                         :team (get-in state [:players player :team])
                         :name (:name seat)
                         :bot? (true? (:bot? seat))
                         :connected? (true? (:connected? seat))
                         :open? (nil? seat)
                         :joinable? (room/joinable-seat? seat)}))
                    game/players)}))

(defn public-room-summary [room]
  {:room-id (:id room)
   :phase (get-in room [:game :phase])
   :player-count (room/human-player-count room)
   :connected-count (count (:connections room))
   :available-count (room/available-seat-count room)})

(defn public-room-summaries [rooms]
  (->> rooms
       vals
       (filter #(and (map? %) (:public? %)))
       (sort-by :created-at)
       (mapv public-room-summary)))

(defn public-rooms-response []
  (edn-response {:ok true
                 :rooms (public-room-summaries @rooms)}))

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

(defn notify-room! [room message]
  (doseq [{:keys [out]} (vals (:connections room))]
    (send-edn! out message)))

(defn delete-room! [room-id reason]
  (let [room-id (normalize-room-id room-id)
        room (get @rooms room-id)]
    (when room
      (notify-room! room {:op :room-closed
                          :room-id room-id
                          :reason reason
                          :message "Room closed"})
      (swap! rooms dissoc room-id)
      (swap! bot-turns dissoc room-id)
      (metric! (case reason
                 :idle :idle-room-deletes
                 :admin :admin-room-deletes
                 :room-deletes))
      room)))

(defn room-empty? [room]
  (empty? (:connections room)))

(defn room-empty-since [room]
  (or (:empty-since room) (:created-at room)))

(defn idle-room? [now room]
  (and (room-empty? room)
       (>= (- now (room-empty-since room)) (idle-room-ms))))

(defn idle-room-ids [rooms now]
  (keep (fn [[room-id room]]
          (when (and (map? room) (idle-room? now room))
            room-id))
        rooms))

(defn close-idle-rooms! []
  (let [now (System/currentTimeMillis)
        room-ids (vec (idle-room-ids @rooms now))]
    (doseq [room-id room-ids]
      (delete-room! room-id :idle))
    room-ids))

(defn start-room-sweeper! []
  (when-not @room-sweeper
    (let [stop (async/chan)]
      (reset! room-sweeper
              {:stop stop
               :done (async/thread
                       (loop []
                         (let [[_ ch] (async/alts!! [stop (async/timeout (idle-room-sweep-ms))])]
                           (when-not (= ch stop)
                             (close-idle-rooms!)
                             (recur)))))}))))

(defn stop-room-sweeper! []
  (when-let [{:keys [stop]} @room-sweeper]
    (async/close! stop)
    (reset! room-sweeper nil)))

(defn admin-delete-room-response [request]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-unauthorized-response)

    (not (origin-allowed? request))
    (response 403 "Forbidden")

    :else
    (let [room-id (admin-delete-room-id (:uri request))]
      (when room-id
        (delete-room! room-id :admin))
      (response 204 "" "text/plain; charset=utf-8"))))

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
                                       (assoc rooms room-id
                                              (room/ensure-room-metadata room'))
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
                                                (room/ensure-room-metadata
                                                 (if (= token (bot-turn-token room))
                                                   (room/advance-bot room)
                                                   room)))
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

(defn create-room! [conn-id out {:keys [name seed public?]}]
  (metric! :room-creates)
  (if (room-limit-reached?)
    (do
      (record-error!)
      (send-edn! out {:op :error :message "Room limit reached"})
      nil)
    (let [room-id (unique-room-id)
          seed (or seed (System/currentTimeMillis))
          room (-> (room/new-room room-id seed public?)
                   (room/join-room {:conn-id conn-id
                                    :out out
                                    :name name})
                   (room/ensure-owner))]
      (swap! rooms assoc room-id room)
      (publish-room! room-id room)
      room-id)))

(defn join-room! [conn-id out {:keys [room-id name player]}]
  (metric! :room-joins)
  (let [room-id (some-> room-id str/upper-case str/trim)
        player (when player (keyword player))]
    (cond
      (not (contains? @rooms room-id))
      (do
        (send-edn! out {:op :error :message "Room not found"})
        false)

      (room-connection-limit-reached? (get @rooms room-id) conn-id)
      (do
        (record-error!)
        (send-edn! out {:op :error :message "Room connection limit reached"})
        false)

      :else
      (try
        (boolean
         (update-room! room-id room/join-room
                       {:conn-id conn-id
                        :out out
                        :name name
                        :player player}))
      (catch Exception e
        (record-error!)
        (send-edn! out {:op :error :message (.getMessage e)})
        false)))))

(defn fill-bots! [room-id out]
  (metric! :fill-bots)
  (cond
    (not room-id)
    (send-edn! out {:op :error :message "Join a room first"})

    (not (contains? @rooms room-id))
    (send-edn! out {:op :error :message "Room not found"})

    :else
    (update-room! room-id room/fill-bots)))

(defn set-room-visibility! [room-id out public?]
  (metric! :room-visibility-updates)
  (cond
    (not room-id)
    (send-edn! out {:op :error :message "Join a room first"})

    (not (contains? @rooms room-id))
    (send-edn! out {:op :error :message "Room not found"})

    :else
    (update-room! room-id room/set-public public?)))

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

(defn leave-room! [conn-id out room-id]
  (when room-id
    (disconnect! conn-id room-id))
  (send-edn! out {:op :left-room
                  :room-id room-id
                  :message "Left room"}))

(defn kick-player! [conn-id room-id out player]
  (metric! :player-kicks)
  (let [player (when player (keyword player))
        room (get @rooms room-id)
        requester (room/connection-player room conn-id)]
    (cond
      (not room)
      (send-edn! out {:op :error :message "Room not found"})

      (not requester)
      (send-edn! out {:op :error :message "Join a room first"})

      (not= requester (:owner room))
      (send-edn! out {:op :error :message "Only the room owner can kick players"})

      (nil? player)
      (send-edn! out {:op :error :message "Choose a player to kick"})

      :else
      (let [kicked-connections (room/player-connections room player)]
        (try
          (update-room! room-id room/kick-player player)
          (doseq [{:keys [out]} (vals kicked-connections)]
            (send-edn! out {:op :kicked
                            :room-id room-id
                            :player player
                            :message "Kicked from room"}))
          true
          (catch Exception e
            (record-error!)
            (send-edn! out {:op :error :message (.getMessage e)})
            false))))))

(defn handle-client-message! [conn-id out session message]
  (case (:op message)
    :create-room
    (when-let [room-id (create-room! conn-id out message)]
      (reset! session {:room-id room-id}))

    :join-room
    (when (join-room! conn-id out message)
      (reset! session {:room-id (some-> (:room-id message) str/upper-case str/trim)}))

    :leave-room
    (do
      (leave-room! conn-id out (:room-id @session))
      (reset! session nil))

    :kick-player
    (kick-player! conn-id (:room-id @session) out (:player message))

    :action
    (handle-action! conn-id (:room-id @session) out message)

    :auto-play
    (auto-play! conn-id (:room-id @session) out)

    :fill-bots
    (fill-bots! (:room-id @session) out)

    :set-room-visibility
    (set-room-visibility! (:room-id @session) out (:public? message))

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

(defn handle-websocket-close! [conn-id in out session]
  (swap! open-websockets disj conn-id)
  (disconnect! conn-id (:room-id @session))
  (async/close! in)
  (async/close! out))

(defn websocket-handler [request]
  (if (admin-stream-enabled? request)
    (admin-stream-response request)
    (if (websocket-limit-reached?)
      (do
        (record-error!)
        (response 503 "Websocket connection limit reached"))
      (let [conn-id (random-uuid)
            in (async/chan 32)
            out (async/chan 32)
            session (atom nil)]
        (swap! open-websockets conj conn-id)
        (http/with-channel request ws
          (http/on-receive ws
                           (fn [raw]
                             (when-not (async/offer! in raw)
                               (record-error!)
                               (send-edn! out {:op :error
                                               :message "Message queue is full"}))))
          (http/on-close ws (fn [_]
                              (runtime/dispatch-close conn-id in out session)))
          (async/thread
            (loop []
              (when-let [message (async/<!! out)]
                (http/send! ws (pr-str message))
                (recur))))
          (async/thread
            (loop []
              (when-let [raw (async/<!! in)]
                (runtime/dispatch-message conn-id out session raw)
                (recur)))))))))

(defn admin-path? [uri]
  (or (= uri "/karbosh/admin")
      (= uri "/karbosh/admin/")))

(defn admin-reload-path? [uri]
  (= uri "/karbosh/admin/reload"))

(defn admin-delete-room-path? [uri]
  (str/starts-with? uri "/karbosh/admin/rooms/"))

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

      (and (= request-method :delete) (admin-delete-room-path? uri))
      (admin-delete-room-response request)

      (and (= request-method :get) (= uri "/karbosh/api/health"))
      (edn-response {:ok true :rooms (count @rooms)})

      (and (= request-method :get) (= uri "/karbosh/api/public-rooms"))
      (public-rooms-response)

      (and (= request-method :get) room-preview-id)
      (room-preview-response room-preview-id)

      (and (= request-method :get) (str/starts-with? uri "/karbosh"))
      (or (static-response uri) (response 404 "Not found"))

      :else
      (response 404 "Not found"))))

(defn install-runtime-handlers! []
  (runtime/install-handler! #'handler)
  (runtime/install-ws-handlers! {:on-message #'handle-raw-message!
                                 :on-close #'handle-websocket-close!}))

(defn start-nrepl-if-enabled! []
  (when (nrepl-enabled?)
    (let [start! (requiring-resolve 'clojure-card-games.karbosh.repl/start!)
          bind (nrepl-bind)
          port (nrepl-port)]
      (start! {:bind bind :port port}))))

(defn stop-nrepl! []
  (when (nrepl-enabled?)
    (when-let [stop! (requiring-resolve 'clojure-card-games.karbosh.repl/stop!)]
      (stop!))))

(defn start! []
  (let [port (parse-port)
        ip (bind-address)]
    (install-runtime-handlers!)
    (start-room-sweeper!)
    (start-nrepl-if-enabled!)
    (reset! server (http/run-server #'runtime/current-handler {:ip ip :port port}))
    (println (str "Karbosh server listening on " ip ":" port))))

(defn stop! []
  (stop-room-sweeper!)
  (stop-nrepl!)
  (when-let [stop @server]
    (stop)
    (reset! server nil)))

(defn -main [& _]
  (start!)
  @(promise))
