(ns clojure-card-games.karbosh.server
  (:gen-class)
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-card-games.karbosh.admin :as admin]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.runtime :as runtime]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.karbosh.page :as page]
            [clojure-card-games.karbosh.storage :as storage]
            [clojure-card-games.karbosh.workbench :as workbench]
            [org.httpkit.server :as http])
  (:import [java.net URI URLDecoder URLEncoder]
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
  '[clojure-card-games.cards
    clojure-card-games.deck
    clojure-card-games.trick
    clojure-card-games.karbosh.shared.cards
    clojure-card-games.karbosh.shared.rules
    clojure-card-games.karbosh.shared.game
    clojure-card-games.karbosh.shared.hand-order
    clojure-card-games.karbosh.audit
    clojure-card-games.karbosh.archive
    clojure-card-games.karbosh.storage
    clojure-card-games.karbosh.parallel
    clojure-card-games.karbosh.analysis
    clojure-card-games.karbosh.bot.config
    clojure-card-games.karbosh.bot.cards
    clojure-card-games.karbosh.bot.inference
    clojure-card-games.karbosh.bot.bid
    clojure-card-games.karbosh.bot.play
    clojure-card-games.karbosh.bot.explain
    clojure-card-games.karbosh.bot
    clojure-card-games.karbosh.solver.play
    clojure-card-games.karbosh.solver.sample
    clojure-card-games.karbosh.solver.pimc
    clojure-card-games.karbosh.room
    clojure-card-games.karbosh.trick-lab
    clojure-card-games.karbosh.page
    clojure-card-games.karbosh.admin
    clojure-card-games.karbosh.workbench
    clojure-card-games.karbosh.server])

(declare broadcast-room!)

(def bot-action-delay-ms 1300)
(def trick-complete-delay-ms 4850)
(def fast-bot-action-delay-ms 220)
(def fast-trick-complete-delay-ms 950)
(def ultra-fast-bot-action-delay-ms 110)
(def ultra-fast-trick-complete-delay-ms 475)

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
  (Long/parseLong (or (System/getenv "KARBOSH_IDLE_ROOM_MS") "300000")))

(defn idle-room-sweep-ms []
  (Long/parseLong (or (System/getenv "KARBOSH_IDLE_ROOM_SWEEP_MS") "60000")))

(defn audit-enabled? []
  (not= "false" (str/lower-case (or (System/getenv "KARBOSH_AUDIT_ENABLED")
                                    "false"))))

(defn audit-dir []
  (or (System/getenv "KARBOSH_AUDIT_DIR") "data/karbosh-audit"))

(defn room-dir []
  (or (System/getenv "KARBOSH_ROOM_DIR") "data/karbosh-rooms"))

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

(def admin-session-cookie-name "karbosh_admin_session")
(def admin-session-max-age-seconds (* 7 24 60 60))

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

(defn redirect-response
  ([location]
   (redirect-response location {}))
  ([location headers]
   {:status 303
    :headers (merge security-headers
                    headers
                    {"Location" location
                     "Content-Type" "text/plain; charset=utf-8"})
    :body "See other"}))

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

(defn authority-uri [authority]
  (try
    (URI. (str "http://" (str/trim (or authority ""))))
    (catch Exception _
      nil)))

(defn loopback-host? [host]
  (contains? #{"localhost" "127.0.0.1" "::1"}
             (some-> host str/lower-case)))

(defn compatible-origin-port? [origin-port request-port]
  (or (= origin-port request-port)
      (= -1 origin-port)
      (= -1 request-port)))

(defn loopback-origin? [raw-origin request-host]
  (when-let [origin (try
                     (URI. raw-origin)
                     (catch Exception _
                       nil))]
    (when-let [host-uri (authority-uri request-host)]
      (and (#{"http" "https"} (some-> (.getScheme origin) str/lower-case))
           (loopback-host? (.getHost origin))
           (loopback-host? (.getHost host-uri))
           (compatible-origin-port? (.getPort origin) (.getPort host-uri))))))

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
             (or (contains? (same-host-origins host) origin)
                 (loopback-origin? raw-origin host))))))

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

(defn sha256-base64url [s]
  (let [digest (doto (MessageDigest/getInstance "SHA-256")
                 (.update (utf8-bytes s)))]
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder))
                     (.digest digest))))

(defn admin-session-cookie-value []
  (when-let [password (admin-password)]
    (sha256-base64url (str "karbosh-admin-session-v1"
                           \u0000
                           (admin-user)
                           \u0000
                           password))))

(defn cookie-map [request]
  (into {}
        (keep (fn [part]
                (let [[k v] (str/split (str/trim part) #"=" 2)]
                  (when-not (str/blank? k)
                    [k (or v "")]))))
        (str/split (or (get-in request [:headers "cookie"]) "") #";")))

(defn admin-cookie-authorized? [request]
  (secure-eq? (admin-session-cookie-value)
              (get (cookie-map request) admin-session-cookie-name)))

(defn admin-authorized? [request]
  (when-let [expected-password (admin-password)]
    (let [[user password] (basic-credentials request)]
      (or (admin-cookie-authorized? request)
          (and (secure-eq? (admin-user) user)
               (secure-eq? expected-password password))))))

(defn https-request? [request]
  (or (= :https (:scheme request))
      (= "https" (some-> (get-in request [:headers "x-forwarded-proto"])
                         str/lower-case))))

(defn admin-session-cookie [request]
  (str admin-session-cookie-name
       "="
       (admin-session-cookie-value)
       "; Path=/karbosh/admin; Max-Age="
       admin-session-max-age-seconds
       "; HttpOnly; SameSite=Lax"
       (when (https-request? request) "; Secure")))

(defn clear-admin-session-cookie []
  (str admin-session-cookie-name
       "=; Path=/karbosh/admin; Max-Age=0; HttpOnly; SameSite=Lax"))

(defn encode-query-value [s]
  (URLEncoder/encode (or s "") "UTF-8"))

(defn safe-admin-return [location]
  (let [location (or (not-empty location) "/karbosh/admin")]
    (if (and (str/starts-with? location "/karbosh/admin")
             (not (str/starts-with? location "//")))
      location
      "/karbosh/admin")))

(defn request-target [{:keys [uri query-string]}]
  (str uri
       (when-not (str/blank? query-string)
         (str "?" query-string))))

(defn admin-login-location [request]
  (str "/karbosh/admin/login?return="
       (encode-query-value (request-target request))))

(defn admin-login-redirect-response [request]
  (redirect-response (admin-login-location request)))

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

(defn request-body-string [request]
  (when-let [body (:body request)]
    (cond
      (string? body) body
      (instance? java.io.InputStream body) (slurp body)
      :else (str body))))

(defn form-params [request]
  (merge (query-params (:query-string request))
         (query-params (request-body-string request))))

(defn admin-login-html [request failed?]
  (let [return-to (safe-admin-return (:return (query-params (:query-string request))))]
    (page/render
     {:title "Karbosh Admin Login"
      :stylesheets ["admin-login.css"]}
     [:main
      [:h1 "Karbosh Admin"]
      [:p "Use a local admin cookie instead of browser Basic auth."]
      (when failed?
        [:p {:class "error"} "Login failed."])
      [:form {:method "post"
              :action "/karbosh/admin/login"
              :autocomplete "off"
              :data-lpignore "true"
              :data-1p-ignore "true"}
       [:input {:type "hidden"
                :name "return"
                :value return-to}]
       [:label
        [:span "Admin password"]
        [:input {:type "password"
                 :name "karbosh_admin_password"
                 :autocomplete "new-password"
                 :data-lpignore "true"
                 :data-1p-ignore "true"
                 :autofocus true}]]
       [:button {:type "submit"} "Start admin session"]]])))

(defn admin-login-response [request]
  (if (admin-password)
    (html-response (admin-login-html request false))
    (admin-disabled-response)))

(defn admin-login-submit-response [request]
  (if-let [expected-password (admin-password)]
    (let [params (form-params request)
          username (or (:username params) (admin-user))
          password (:karbosh_admin_password params)
          return-to (safe-admin-return (:return params))]
      (if (and (secure-eq? (admin-user) username)
               (secure-eq? expected-password password))
        (redirect-response return-to
                           {"Set-Cookie" (admin-session-cookie request)})
        (response 401
                  (admin-login-html (assoc request
                                           :query-string
                                           (str "return="
                                                (encode-query-value return-to)))
                                    true)
                  "text/html; charset=utf-8")))
    (admin-disabled-response)))

(defn admin-logout-response [_request]
  (redirect-response "/karbosh/admin/login"
                     {"Set-Cookie" (clear-admin-session-cookie)}))

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

(defn admin-room-snapshot-id [uri]
  (let [prefix "/karbosh/admin/rooms/"
        suffix "/snapshot"]
    (when (and (str/starts-with? uri prefix)
               (str/ends-with? uri suffix))
      (normalize-room-id
       (decode-query-value
        (subs uri (count prefix) (- (count uri) (count suffix))))))))

(defn admin-room-snapshot-edn-id [uri]
  (let [prefix "/karbosh/admin/rooms/"
        suffix "/snapshot.edn"]
    (when (and (str/starts-with? uri prefix)
               (str/ends-with? uri suffix))
      (normalize-room-id
       (decode-query-value
        (subs uri (count prefix) (- (count uri) (count suffix))))))))

(defn parse-hand-index [s]
  (try
    (Long/parseLong (str s))
    (catch Exception _
      nil)))

(defn admin-room-hand-detail-id [uri]
  (let [prefix "/karbosh/admin/rooms/"]
    (when (str/starts-with? uri prefix)
      (let [[room-id snapshot hands hand-index & extra]
            (str/split (subs uri (count prefix)) #"/")]
        (when (and room-id
                   (= "snapshot" snapshot)
                   (= "hands" hands)
                   hand-index
                   (nil? extra))
          (when-let [hand-index (parse-hand-index hand-index)]
            {:room-id (normalize-room-id (decode-query-value room-id))
             :hand-index hand-index}))))))

(defn admin-room-trick-analysis-id [uri]
  (let [prefix "/karbosh/admin/rooms/"]
    (when (str/starts-with? uri prefix)
      (let [[room-id snapshot hands hand-index tricks trick-index analysis & extra]
            (str/split (subs uri (count prefix)) #"/")]
        (when (and room-id
                   (= "snapshot" snapshot)
                   (= "hands" hands)
                   hand-index
                   (= "tricks" tricks)
                   trick-index
                   (= "analysis" analysis)
                   (nil? extra))
          (when-let [hand-index (parse-hand-index hand-index)]
            (when-let [trick-index (parse-hand-index trick-index)]
              {:room-id (normalize-room-id (decode-query-value room-id))
               :hand-index hand-index
               :trick-index trick-index})))))))

(defn admin-workbench-id [uri]
  (let [prefix "/karbosh/admin/workbench/"]
    (when (str/starts-with? uri prefix)
      (let [[room-id & extra] (str/split (subs uri (count prefix)) #"/")]
        (when (and room-id (nil? extra))
          (normalize-room-id (decode-query-value room-id)))))))

(defn admin-workbench-index-path? [uri]
  (or (= uri "/karbosh/admin/workbench")
      (= uri "/karbosh/admin/workbench/")))

(defn admin-history-path? [uri]
  (= uri "/karbosh/admin/history"))

(defn admin-game-snapshot-path [uri suffix]
  (let [prefix "/karbosh/admin/history/"]
    (when (and (str/starts-with? uri prefix)
               (str/ends-with? uri suffix))
      (let [path (subs uri
                       (count prefix)
                       (- (count uri) (count suffix)))
            [room-id seed timestamp & extra] (str/split path #"/")]
        (when (and room-id seed (nil? extra))
          (cond-> {:room-id (normalize-room-id (decode-query-value room-id))
                   :seed (decode-query-value seed)}
            timestamp (assoc :timestamp (decode-query-value timestamp))))))))

(defn admin-game-snapshot-id [uri]
  (admin-game-snapshot-path uri "/snapshot"))

(defn admin-game-snapshot-edn-id [uri]
  (admin-game-snapshot-path uri "/snapshot.edn"))

(defn admin-game-hand-detail-id [uri]
  (let [prefix "/karbosh/admin/history/"]
    (when (str/starts-with? uri prefix)
      (let [[room-id seed third fourth fifth sixth & extra]
            (str/split (subs uri (count prefix)) #"/")
            timestamp (when-not (= "snapshot" third) third)
            snapshot (if timestamp fourth third)
            hands (if timestamp fifth fourth)
            hand-index (if timestamp sixth fifth)]
        (when (and room-id
                   seed
                   (= "snapshot" snapshot)
                   (= "hands" hands)
                   hand-index
                   (nil? extra))
          (when-let [hand-index (parse-hand-index hand-index)]
            (cond-> {:room-id (normalize-room-id (decode-query-value room-id))
                     :seed (decode-query-value seed)
                     :hand-index hand-index}
              timestamp (assoc :timestamp (decode-query-value timestamp)))))))))

(defn admin-game-trick-analysis-id [uri]
  (let [prefix "/karbosh/admin/history/"]
    (when (str/starts-with? uri prefix)
      (let [[room-id seed third fourth fifth sixth seventh eighth ninth & extra]
            (str/split (subs uri (count prefix)) #"/")
            timestamp (when-not (= "snapshot" third) third)
            snapshot (if timestamp fourth third)
            hands (if timestamp fifth fourth)
            hand-index (if timestamp sixth fifth)
            tricks (if timestamp seventh sixth)
            trick-index (if timestamp eighth seventh)
            analysis (if timestamp ninth eighth)]
        (when (and room-id
                   seed
                   (= "snapshot" snapshot)
                   (= "hands" hands)
                   hand-index
                   (= "tricks" tricks)
                   trick-index
                   (= "analysis" analysis)
                   (nil? extra))
          (when-let [hand-index (parse-hand-index hand-index)]
            (when-let [trick-index (parse-hand-index trick-index)]
              (cond-> {:room-id (normalize-room-id (decode-query-value room-id))
                       :seed (decode-query-value seed)
                       :hand-index hand-index
                       :trick-index trick-index}
                timestamp (assoc :timestamp (decode-query-value timestamp))))))))))

(declare start-room-sweeper! install-runtime-handlers! start-nrepl-if-enabled!)

(defn start-audit! []
  (audit/start! {:enabled? (audit-enabled?)
                 :dir (audit-dir)}))

(defn audit-current-rooms! [event-type]
  (doseq [room (vals @rooms)
          :when (and (map? room)
                     (audit/played-room? room))]
    (audit/record-room! event-type room)))

(defn save-room! [room]
  (when (map? room)
    (storage/write-room! (room-dir) room)))

(defn save-current-rooms! []
  (doseq [room (vals @rooms)
          :when (map? room)]
    (save-room! room)))

(defn load-room [room-id]
  (some-> (or (storage/read-room (room-dir) room-id)
              (:room (audit/latest-room-record (audit-dir) room-id)))
          room/ensure-room-metadata))

(defn durable-room-exists? [room-id]
  (or (storage/room-exists? (room-dir) room-id)
      (some? (audit/latest-room-record (audit-dir) room-id))))

(defn delete-durable-room! [room-id]
  (storage/delete-room! (room-dir) room-id))

(defn ensure-room-loaded! [room-id]
  (when room-id
    (or (get @rooms room-id)
        (when-let [room (load-room room-id)]
          (get (swap! rooms #(if (contains? % room-id)
                               %
                               (assoc % room-id room)))
               room-id)))))

(defn room-by-id [room-id]
  (when room-id
    (or (get @rooms room-id)
        (load-room room-id))))

(defn stored-room [room-id]
  (when room-id
    (storage/read-room (room-dir) room-id)))

(defn durable-rooms []
  (->> (storage/room-files (room-dir))
       (keep #(load-room (storage/room-id-from-file %)))
       vec))

(defn durable-room-record [room]
  (audit/room-record :room-durable
                     room
                     (or (:updated-at room) (:created-at room))))

(defn durable-room-records []
  (mapv durable-room-record (durable-rooms)))

(defn latest-records-by-room [records]
  (->> records
       (reduce (fn [by-room record]
                 (let [room-id (:room-id record)
                       existing (get by-room room-id)]
                   (if (or (nil? existing)
                           (> (or (:logged-at record) 0)
                              (or (:logged-at existing) 0)))
                     (assoc by-room room-id record)
                     by-room)))
               {})
       vals
       (sort-by :logged-at >)
       vec))

(defn durable-entry-room [room entry]
  (-> room
      (dissoc :games :connections)
      (assoc :game (:game entry)
             :seed (:seed entry)
             :game-index (:game-index entry)
             :game-started-at (:started-at entry))))

(defn durable-game-records-for-room [room]
  (let [completed (mapv #(audit/room-record :room-game
                                            (durable-entry-room room %)
                                            (or (:completed-at %)
                                                (:updated-at room)
                                                (:created-at room)))
                        (:games room))
        current (when (audit/played-room? room)
                  [(durable-room-record (dissoc room :games))])]
    (into completed current)))

(defn durable-game-records []
  (->> (durable-rooms)
       (mapcat durable-game-records-for-room)
       audit/game-records-from-candidates))

(defn historical-room-records []
  (latest-records-by-room
   (concat (audit/latest-room-records (audit-dir))
           (durable-room-records))))

(defn archived-game-records []
  (audit/game-records-from-candidates
   (concat (audit/game-history-records (audit-dir))
           (durable-game-records))))

(defn historical-room-record [room-id]
  (or (some #(when (= room-id (:room-id %)) %)
            (durable-room-records))
      (audit/latest-room-record (audit-dir) room-id)))

(defn live-game-record [room-id seed timestamp]
  (when-let [room (get @rooms room-id)]
    (when (and (= (str seed) (str (get-in room [:game :initial-seed])))
               (or (nil? timestamp)
                   (= (str timestamp)
                      (str (or (:game-started-at room)
                               (:created-at room))))))
      (audit/room-record :room-live room))))

(defn historical-game-record [room-id seed timestamp]
  (or (some (fn [record]
              (when (and (= room-id (:room-id record))
                         (= (str seed)
                            (str (get-in record [:room :game :initial-seed])))
                         (or (nil? timestamp)
                             (= (str timestamp)
                                (str (or (get-in record [:room :game-started-at])
                                         (get-in record [:room :created-at])
                                         (:logged-at record))))))
                record))
            (durable-game-records))
      (audit/room-game-record (audit-dir) room-id seed timestamp)))

(defn game-history-record [room-id seed timestamp]
  (or (live-game-record room-id seed timestamp)
      (historical-game-record room-id seed timestamp)))

(defn room-snapshot-url [room-id]
  (str "/karbosh/admin/rooms/" room-id "/snapshot"))

(defn game-snapshot-url [{:keys [room-id seed timestamp]}]
  (str "/karbosh/admin/history/" room-id "/" seed
       (when timestamp (str "/" timestamp))
       "/snapshot"))

(defn api-game-url [{:keys [room-id seed timestamp]}]
  (str "/karbosh/api/rooms/" room-id "/games/" seed "/" timestamp))

(defn api-hand-url [{:keys [hand-index] :as id}]
  (str (api-game-url id) "/hands/" hand-index))

(defn api-trick-url [{:keys [trick-index] :as id}]
  (str (api-hand-url id) "/tricks/" trick-index))

(defn edn-error-response [status body]
  (response status
            (pr-str (assoc body :ok false))
            "application/edn; charset=utf-8"))

(defn game-id-from-record [record]
  {:room-id (:room-id record)
   :seed (audit/game-seed record)
   :timestamp (audit/game-timestamp record)})

(defn hand-links [game-id room]
  (mapv (fn [hand]
          {:hand-index (:hand-index hand)
           :href (api-hand-url (assoc game-id :hand-index (:hand-index hand)))})
        (admin/room-hands room)))

(defn trick-links [game-id hand]
  (let [completed-count (count (:completed-tricks hand))
        current-count (if (seq (:current-trick hand)) 1 0)]
    (mapv (fn [trick-index]
            {:trick-index trick-index
             :href (api-trick-url (assoc game-id
                                          :hand-index (:hand-index hand)
                                          :trick-index trick-index))})
          (range (+ completed-count current-count)))))

(defn room-snapshot-data [room-id]
  (if-let [room (or (get @rooms room-id)
                    (stored-room room-id))]
    (let [room (audit/sanitize-room room)]
      {:ok true
       :kind :room-snapshot
       :durable? (not (contains? @rooms room-id))
       :room-id room-id
       :room room
       :view (game/admin-view (:game room) (:seats room))
       :links {:games (str "/karbosh/api/rooms/" room-id "/games")
               :snapshot (str "/karbosh/api/rooms/" room-id "/snapshot")
               :admin-snapshot (room-snapshot-url room-id)}})
    (when-let [record (historical-room-record room-id)]
      (let [room (:room record)]
        {:ok true
         :kind :room-snapshot
         :historical? true
         :record (dissoc record :room)
         :room-id room-id
         :room room
         :view (game/admin-view (:game room) (:seats room))
         :links {:games (str "/karbosh/api/rooms/" room-id "/games")
                 :snapshot (str "/karbosh/api/rooms/" room-id "/snapshot")
                 :admin-snapshot (room-snapshot-url room-id)}}))))

(defn room-game-candidate-records [room-id]
  (let [room-records (when-let [room (or (get @rooms room-id)
                                         (stored-room room-id))]
                       (durable-game-records-for-room (audit/sanitize-room room)))
        audit-records (audit/game-candidate-records
                       (audit/room-file (audit-dir) room-id))]
    (audit/game-records-from-candidates (concat room-records audit-records))))

(defn game-summary [record]
  (let [{:keys [room-id seed timestamp] :as id} (game-id-from-record record)
        room (:room record)
        game (:game room)
        hands (admin/room-hands room)]
    {:room-id room-id
     :seed seed
     :timestamp timestamp
     :game-index (:game-index room)
     :phase (:phase game)
     :winner (:winner game)
     :score (:scores game)
     :hand-count (count hands)
     :updated-at (or (:updated-at room) (:logged-at record))
     :links {:game (api-game-url id)
             :snapshot (game-snapshot-url id)
             :raw-snapshot (str (game-snapshot-url id) ".edn")
             :hands (hand-links id room)}}))

(defn game-data [record]
  (let [{:keys [room-id seed timestamp] :as id} (game-id-from-record record)
        room (:room record)
        game (:game room)]
    {:ok true
     :kind :game
     :room-id room-id
     :seed seed
     :timestamp timestamp
     :record (dissoc record :room)
     :room (select-keys room [:id :public? :created-at :updated-at
                              :game-index :game-started-at])
     :seats (:seats room)
     :game game
     :view (game/admin-view game (:seats room))
     :links {:self (api-game-url id)
             :room-snapshot (str "/karbosh/api/rooms/" room-id "/snapshot")
             :admin-snapshot (game-snapshot-url id)
             :hands (hand-links id room)}}))

(defn hand-data [record hand-index]
  (let [{:keys [room-id] :as id} (game-id-from-record record)
        room (:room record)]
    (if-let [hand (admin/room-hand room hand-index)]
      {:ok true
       :kind :hand
       :room-id room-id
       :seed (:seed id)
       :timestamp (:timestamp id)
       :hand-index hand-index
       :hand hand
       :links {:self (api-hand-url (assoc id :hand-index hand-index))
               :game (api-game-url id)
               :admin-hand (admin/hand-detail-url (game-snapshot-url id) hand)
               :tricks (trick-links id hand)}}
      (edn-error-response 404
                          {:room-id room-id
                           :seed (:seed id)
                           :timestamp (:timestamp id)
                           :hand-index hand-index
                           :message "Hand not found"}))))

(defn indexed-trick [hand trick-index]
  (let [completed (:completed-tricks hand)
        completed-count (count completed)]
    (cond
      (< -1 trick-index completed-count)
      {:status :completed
       :trick (nth completed trick-index)}

      (and (= trick-index completed-count)
           (seq (:current-trick hand)))
      {:status :current
       :trick (:current-trick hand)}

      :else nil)))

(defn ring-response? [x]
  (and (map? x)
       (integer? (:status x))
       (contains? x :headers)))

(defn trick-data [record hand-index trick-index]
  (let [{:keys [room-id] :as id} (game-id-from-record record)
        room (:room record)]
    (if-let [hand (admin/room-hand room hand-index)]
      (if-let [{:keys [status trick]} (indexed-trick hand trick-index)]
        (let [trump (:trump hand)
              winning-play (rules/winning-play trick trump)
              winning-player (:player winning-play)]
          {:ok true
           :kind :trick
           :room-id room-id
           :seed (:seed id)
           :timestamp (:timestamp id)
           :hand-index hand-index
           :trick-index trick-index
           :trick-status status
           :trump trump
           :lead (rules/trick-lead trick trump)
           :winning-play winning-play
           :winning-player winning-player
           :winning-team (get-in room [:game :players winning-player :team])
           :trick trick
           :hand (select-keys hand [:hand-index :bid :trump :tricks :points
                                    :scores-after :phase :current?])
           :links {:self (api-trick-url (assoc id
                                                :hand-index hand-index
                                                :trick-index trick-index))
                   :hand (api-hand-url (assoc id :hand-index hand-index))
                   :game (api-game-url id)
                   :admin-analysis (str (admin/hand-detail-url
                                         (game-snapshot-url id)
                                         hand)
                                        "/tricks/" trick-index "/analysis")}})
        (edn-error-response 404
                            {:room-id room-id
                             :seed (:seed id)
                             :timestamp (:timestamp id)
                             :hand-index hand-index
                             :trick-index trick-index
                             :message "Trick not found"}))
      (edn-error-response 404
                          {:room-id room-id
                           :seed (:seed id)
                           :timestamp (:timestamp id)
                           :hand-index hand-index
                           :message "Hand not found"}))))

(defn api-room-route [uri]
  (let [prefix "/karbosh/api/rooms/"]
    (when (str/starts-with? uri prefix)
      (let [[room-id & path] (map decode-query-value
                                  (str/split (subs uri (count prefix)) #"/"))
            room-id (normalize-room-id room-id)]
        (when room-id
          (let [[a b c d e f & extra] path]
            (cond
              (and (= "snapshot" a) (nil? b))
              {:kind :room-snapshot :room-id room-id}

              (and (= "games" a) (nil? b))
              {:kind :room-games :room-id room-id}

              (and (= "games" a) b c (nil? d))
              {:kind :game :room-id room-id :seed b :timestamp c}

              (and (= "games" a) b c (= "hands" d) e (nil? f))
              (when-let [hand-index (parse-hand-index e)]
                {:kind :hand
                 :room-id room-id
                 :seed b
                 :timestamp c
                 :hand-index hand-index})

              (and (= "games" a) b c (= "hands" d) e (= "tricks" f) (first extra)
                   (nil? (second extra)))
              (when-let [hand-index (parse-hand-index e)]
                (when-let [trick-index (parse-hand-index (first extra))]
                  {:kind :trick
                   :room-id room-id
                   :seed b
                   :timestamp c
                   :hand-index hand-index
                   :trick-index trick-index})))))))))

(defn api-room-data-response [{:keys [kind room-id seed timestamp hand-index trick-index]}]
  (case kind
    :room-snapshot
    (if-let [data (room-snapshot-data room-id)]
      (edn-response data)
      (edn-error-response 404 {:room-id room-id
                               :message "Room not found"}))

    :room-games
    (let [records (room-game-candidate-records room-id)]
      (if (or (seq records) (room-snapshot-data room-id))
        (edn-response {:ok true
                       :kind :room-games
                       :room-id room-id
                       :games (mapv game-summary records)
                       :links {:room-snapshot (str "/karbosh/api/rooms/"
                                                   room-id
                                                   "/snapshot")}})
        (edn-error-response 404 {:room-id room-id
                                 :message "Room not found"})))

    :game
    (if-let [record (game-history-record room-id seed timestamp)]
      (edn-response (game-data record))
      (edn-error-response 404 {:room-id room-id
                               :seed seed
                               :timestamp timestamp
                               :message "Game not found"}))

    :hand
    (if-let [record (game-history-record room-id seed timestamp)]
      (let [data (hand-data record hand-index)]
        (if (ring-response? data) data (edn-response data)))
      (edn-error-response 404 {:room-id room-id
                               :seed seed
                               :timestamp timestamp
                               :message "Game not found"}))

    :trick
    (if-let [record (game-history-record room-id seed timestamp)]
      (let [data (trick-data record hand-index trick-index)]
        (if (ring-response? data) data (edn-response data)))
      (edn-error-response 404 {:room-id room-id
                               :seed seed
                               :timestamp timestamp
                               :message "Game not found"}))))

(defn admin-history-response [request]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-login-redirect-response request)

    :else
    (html-response
     (admin/render-history {:rooms @rooms
                            :records (archived-game-records)}))))

(defn admin-game-snapshot-edn-response [_request {:keys [room-id seed timestamp]}]
  (if-let [record (game-history-record room-id seed timestamp)]
    (let [room (:room record)]
      (edn-response {:ok true
                     :historical? true
                     :record (dissoc record :room)
                     :room-id room-id
                     :seed (get-in room [:game :initial-seed])
                     :timestamp (audit/game-timestamp record)
                     :room room
                     :view (game/admin-view (:game room) (:seats room))}))
    (response 404
              (pr-str {:ok false
                       :room-id room-id
                       :seed seed
                       :timestamp timestamp
                       :message "Game not found"})
              "application/edn; charset=utf-8")))

(defn admin-game-snapshot-response [request {:keys [room-id seed timestamp]}]
  (if-let [record (game-history-record room-id seed timestamp)]
    (html-response (admin/render-room-snapshot
                    (:room record)
                    (game-snapshot-url {:room-id room-id
                                        :seed seed
                                        :timestamp timestamp})))
    (response 404 "Game not found")))

(defn admin-game-hand-detail-response
  [request {:keys [room-id seed timestamp hand-index] :as id}]
  (if-let [record (game-history-record room-id seed timestamp)]
    (let [room (:room record)
          snapshot-url (game-snapshot-url id)]
      (if (admin/room-hand room hand-index)
        (html-response (admin/render-room-hand-detail room hand-index snapshot-url))
        (response 404 "Hand not found")))
    (response 404 "Game not found")))

(defn admin-game-trick-analysis-response
  [request {:keys [room-id seed timestamp hand-index trick-index] :as id}]
  (if-let [record (game-history-record room-id seed timestamp)]
    (let [room (:room record)
          snapshot-url (game-snapshot-url id)]
      (if (admin/room-hand room hand-index)
        (html-response
         (admin/render-trick-analysis room
                                      hand-index
                                      trick-index
                                      snapshot-url
                                      (query-params (:query-string request))))
        (response 404 "Hand not found")))
    (response 404 "Game not found")))

(defn admin-dashboard-response [request]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-login-redirect-response request)

    :else
    (html-response
     (admin/render-dashboard {:rooms @rooms
                              :selected-room-id (selected-admin-room-id request)
                              :historical-room-records (historical-room-records)
                              :metrics @metrics
                              :pending-bot-count (count @bot-turns)
                              :open-websocket-count (count @open-websockets)
                              :limits {:max-rooms (max-rooms)
                                       :max-room-connections (max-room-connections)
                                       :max-websocket-connections (max-websocket-connections)
                                       :max-message-bytes (max-message-bytes)
                                       :idle-room-ms (idle-room-ms)}
                              :started-at (:started-at @metrics)}))))

(defn admin-dashboard-main-html
  ([request]
   (admin-dashboard-main-html request {:include-historical? true}))
  ([request {:keys [include-historical?]
             :or {include-historical? true}}]
   (admin/render-dashboard-main-html
    (cond-> {:rooms @rooms
             :selected-room-id (selected-admin-room-id request)
             :include-historical? include-historical?
             :metrics @metrics
             :pending-bot-count (count @bot-turns)
             :open-websocket-count (count @open-websockets)
             :limits {:max-rooms (max-rooms)
                      :max-room-connections (max-room-connections)
                      :max-websocket-connections (max-websocket-connections)
                      :max-message-bytes (max-message-bytes)
                      :idle-room-ms (idle-room-ms)}
             :started-at (:started-at @metrics)}
      include-historical? (assoc :historical-room-records
                                 (historical-room-records))))))

(defn admin-dashboard-stream-html [request]
  (admin-dashboard-main-html request {:include-historical? false}))

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
        (http/send! ws (admin-dashboard-stream-html request))
        (async/thread
          (while (not @stop)
            (Thread/sleep 3000)
            (when-not @stop
              (try
                (when-not (http/send! ws (admin-dashboard-stream-html request))
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
    (start-audit!)
    (audit-current-rooms! :reload-snapshot)
    (start-room-sweeper!)
    (install-runtime-handlers!)
    (start-nrepl-if-enabled!)
    (refresh-room-view-state!)
    (save-current-rooms!)
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
       (filter #(and (map? %)
                     (:public? %)
                     (seq (:connections %))))
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
  (if-let [room (room-by-id room-id)]
    (edn-response (assoc (room-preview room) :ok true))
    (response 404
              (pr-str {:ok false
                       :room-id room-id
                       :message "Room not found"})
              "application/edn; charset=utf-8")))

(defn admin-room-snapshot-edn-response [_request room-id]
  (if-let [room (or (get @rooms room-id)
                    (stored-room room-id))]
    (edn-response {:ok true
                   :durable? (not (contains? @rooms room-id))
                   :room-id room-id
                   :room (audit/sanitize-room room)
                   :view (game/admin-view (:game room) (:seats room))})
    (if-let [record (historical-room-record room-id)]
      (let [room (:room record)]
        (edn-response {:ok true
                       :historical? true
                       :record (dissoc record :room)
                       :room-id room-id
                       :room room
                       :view (game/admin-view (:game room) (:seats room))}))
      (response 404
                (pr-str {:ok false
                         :room-id room-id
                         :message "Room not found"})
                "application/edn; charset=utf-8"))))

(defn admin-room-snapshot-response [request room-id]
  (if-let [room (or (get @rooms room-id)
                    (stored-room room-id))]
    (html-response (admin/render-room-snapshot
                    (audit/sanitize-room room)
                    (room-snapshot-url room-id)))
    (if-let [record (historical-room-record room-id)]
      (html-response (admin/render-room-snapshot
                      (:room record)
                      (room-snapshot-url room-id)))
      (response 404 "Room not found"))))

(defn admin-room-hand-detail-response [request {:keys [room-id hand-index]}]
  (if-let [room (or (get @rooms room-id)
                    (stored-room room-id))]
    (let [room (audit/sanitize-room room)]
      (if (admin/room-hand room hand-index)
        (html-response
         (admin/render-room-hand-detail room hand-index (room-snapshot-url room-id)))
        (response 404 "Hand not found")))
    (if-let [record (historical-room-record room-id)]
      (let [room (:room record)]
        (if (admin/room-hand room hand-index)
          (html-response
           (admin/render-room-hand-detail room hand-index (room-snapshot-url room-id)))
          (response 404 "Hand not found")))
      (response 404 "Room not found"))))

(defn admin-room-trick-analysis-response
  [request {:keys [room-id hand-index trick-index]}]
  (if-let [room (or (get @rooms room-id)
                    (stored-room room-id))]
    (let [room (audit/sanitize-room room)]
      (if (admin/room-hand room hand-index)
        (html-response
         (admin/render-trick-analysis room
                                      hand-index
                                      trick-index
                                      (room-snapshot-url room-id)
                                      (query-params (:query-string request))))
        (response 404 "Hand not found")))
    (if-let [record (historical-room-record room-id)]
      (let [room (:room record)]
        (if (admin/room-hand room hand-index)
          (html-response
           (admin/render-trick-analysis room
                                        hand-index
                                        trick-index
                                        (room-snapshot-url room-id)
                                        (query-params (:query-string request))))
          (response 404 "Hand not found")))
      (response 404 "Room not found"))))

(defn workbench-source-room [room-id]
  (or (room-by-id room-id)
      (some-> (historical-room-record room-id) :room)))

(defn load-workbench-bookmarks! [room-id]
  (doseq [record (audit/room-records (audit-dir) room-id)]
    (when-let [bookmark (workbench/bookmark-from-record record)]
      (workbench/install-bookmark! bookmark))))

(defn persist-workbench-bookmark! [session]
  (when-let [bookmark-id (:last-bookmark-id session)]
    (when-let [bookmark (workbench/bookmark-by-id (get-in session [:room :id])
                                                  bookmark-id)]
      (audit/append-record! (audit-dir) (workbench/bookmark-record bookmark)))))

(defn workbench-bookmarks []
  (->> (concat (keep workbench/bookmark-from-record
                     (audit/all-room-records (audit-dir)))
               (mapcat val @workbench/bookmarks*))
       (reduce (fn [bookmarks bookmark]
                 (assoc bookmarks (:id bookmark) bookmark))
               {})
       vals
       (sort-by #(or (:created-at %) 0) >)
       vec))

(defn admin-workbench-index-response [request]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-login-redirect-response request)

    :else
    (if-let [room-id (some-> (query-params (:query-string request))
                             :room
                             normalize-room-id)]
      (redirect-response (str "/karbosh/admin/workbench/" room-id))
      (html-response
       (admin/render-workbench-index {:rooms @rooms
                                      :records (historical-room-records)
                                      :bookmarks (workbench-bookmarks)})))))

(defn admin-workbench-response [request room-id]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-login-redirect-response request)

    :else
    (if-let [room (workbench-source-room room-id)]
      (do
        (load-workbench-bookmarks! room-id)
        (html-response
         (workbench/render (workbench/ensure-session! room-id room))))
      (response 404 "Room not found"))))

(defn admin-workbench-action-response [request room-id]
  (cond
    (not (admin-password))
    (admin-disabled-response)

    (not (admin-authorized? request))
    (admin-login-redirect-response request)

    :else
    (if-let [room (workbench-source-room room-id)]
      (let [params (form-params request)
            session (workbench/handle-action! room-id room params)]
        (when (#{"bookmark" "capture-room"} (:action params))
          (persist-workbench-bookmark! session))
        (redirect-response (str "/karbosh/admin/workbench/" room-id)))
      (response 404 "Room not found"))))

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
      (if (audit/played-room? room)
        (audit/record-room! (keyword "room-delete" (name reason)) room)
        (audit/prune-room-records! (audit-dir) room-id))
      (delete-durable-room! room-id)
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

(defn unload-room! [room-id reason]
  (let [room-id (normalize-room-id room-id)
        room (get @rooms room-id)]
    (when room
      (save-room! room)
      (metric! (case reason
                 :idle :idle-room-unloads
                 :room-unloads))
      (swap! rooms dissoc room-id)
      (swap! bot-turns dissoc room-id)
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
      (unload-room! room-id :idle))
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
     (room/speed-mode room)
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
  (let [speed-mode (room/speed-mode room)]
    (if (completed-trick-state? room)
      (case speed-mode
        :ultra-fast ultra-fast-trick-complete-delay-ms
        :fast fast-trick-complete-delay-ms
        trick-complete-delay-ms)
      (case speed-mode
        :ultra-fast ultra-fast-bot-action-delay-ms
        :fast fast-bot-action-delay-ms
        bot-action-delay-ms))))

(defn publish-room! [room-id room]
  (save-room! room)
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
    (if (or (contains? @rooms id)
            (durable-room-exists? id))
      (recur (room/random-room-id))
      id)))

(defn room-limit-reached? []
  (>= (count @rooms) (max-rooms)))

(defn room-connection-limit-reached? [room conn-id]
  (and (not (contains? (:connections room) conn-id))
       (>= (count (:connections room)) (max-room-connections))))

(defn websocket-limit-reached? []
  (>= (count @open-websockets) (max-websocket-connections)))

(defn long-range? [n]
  (<= Long/MIN_VALUE n Long/MAX_VALUE))

(defn parse-room-seed [seed]
  (cond
    (nil? seed) nil

    (integer? seed)
    (when (long-range? seed)
      (long seed))

    (string? seed)
    (let [seed (str/trim seed)]
      (when (re-matches #"[+-]?\d+" seed)
        (try
          (Long/parseLong seed)
          (catch NumberFormatException _
            nil))))

    :else nil))

(defn invalid-room-seed? [seed]
  (and (some? seed)
       (nil? (parse-room-seed seed))))

(defn create-room! [conn-id out {:keys [name seed public? fast-mode? speed-mode]}]
  (metric! :room-creates)
  (cond
    (room-limit-reached?)
    (do
      (record-error!)
      (send-edn! out {:op :error :message "Room limit reached"})
      nil)

    (invalid-room-seed? seed)
    (do
      (record-error!)
      (send-edn! out {:op :error :message "Seed must be an integer"})
      nil)

    :else
    (let [room-id (unique-room-id)
          seed (or (parse-room-seed seed) (room/random-seed))
          room (-> (room/new-room room-id seed public? (or speed-mode fast-mode?))
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
        player (when player (keyword player))
        room (ensure-room-loaded! room-id)]
    (cond
      (nil? room)
      (do
        (send-edn! out {:op :error :message "Room not found"})
        false)

      (room-connection-limit-reached? room conn-id)
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

(defn seat-bot! [room-id out player bot-name]
  (metric! :seat-bots)
  (let [player (when player (keyword player))]
    (cond
      (not room-id)
      (send-edn! out {:op :error :message "Join a room first"})

      (not (contains? @rooms room-id))
      (send-edn! out {:op :error :message "Room not found"})

      (nil? player)
      (send-edn! out {:op :error :message "Choose a seat"})

      (str/blank? (or bot-name ""))
      (send-edn! out {:op :error :message "Choose a bot"})

      :else
      (try
        (update-room! room-id room/seat-named-bot player bot-name)
        (catch Exception e
          (record-error!)
          (send-edn! out {:op :error :message (.getMessage e)}))))))

(defn set-room-visibility! [room-id out public?]
  (metric! :room-visibility-updates)
  (cond
    (not room-id)
    (send-edn! out {:op :error :message "Join a room first"})

    (not (contains? @rooms room-id))
    (send-edn! out {:op :error :message "Room not found"})

    :else
    (update-room! room-id room/set-public public?)))

(defn set-fast-mode! [room-id out mode]
  (metric! :room-fast-mode-updates)
  (cond
    (not room-id)
    (send-edn! out {:op :error :message "Join a room first"})

    (not (contains? @rooms room-id))
    (send-edn! out {:op :error :message "Room not found"})

    :else
    (update-room! room-id room/set-speed-mode mode)))

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

    :seat-bot
    (seat-bot! (:room-id @session) out (:player message) (:bot-name message))

    :set-room-visibility
    (set-room-visibility! (:room-id @session) out (:public? message))

    :set-fast-mode
    (set-fast-mode! (:room-id @session) out (or (:speed-mode message)
                                                (:fast-mode? message)))

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

(defn admin-login-path? [uri]
  (= uri "/karbosh/admin/login"))

(defn admin-logout-path? [uri]
  (= uri "/karbosh/admin/logout"))

(defn admin-reload-path? [uri]
  (= uri "/karbosh/admin/reload"))

(defn admin-delete-room-path? [uri]
  (str/starts-with? uri "/karbosh/admin/rooms/"))

(defn handler [{:keys [uri request-method] :as request}]
  (let [room-preview-id (room-preview-id uri)
        snapshot-room-id (admin-room-snapshot-id uri)
        snapshot-edn-room-id (admin-room-snapshot-edn-id uri)
        room-hand-detail-id (admin-room-hand-detail-id uri)
        room-trick-analysis-id (admin-room-trick-analysis-id uri)
        workbench-id (admin-workbench-id uri)
        game-snapshot-id (admin-game-snapshot-id uri)
        game-snapshot-edn-id (admin-game-snapshot-edn-id uri)
        game-hand-detail-id (admin-game-hand-detail-id uri)
        game-trick-analysis-id (admin-game-trick-analysis-id uri)
        api-room-route (api-room-route uri)]
    (cond
      (and (= request-method :get) (= uri "/karbosh/ws"))
      (if (origin-allowed? request)
        (websocket-handler request)
        (response 403 "Forbidden"))

      (and (= request-method :get) (admin-login-path? uri))
      (admin-login-response request)

      (and (= request-method :post) (admin-login-path? uri))
      (admin-login-submit-response request)

      (and (= request-method :get) (admin-logout-path? uri))
      (admin-logout-response request)

      (and (= request-method :get) (admin-path? uri))
      (admin-dashboard-response request)

      (and (= request-method :get) (admin-history-path? uri))
      (admin-history-response request)

      (and (= request-method :post) (admin-reload-path? uri))
      (admin-reload-response request)

      (and (= request-method :delete) (admin-delete-room-path? uri))
      (admin-delete-room-response request)

      (and (= request-method :get) (admin-workbench-index-path? uri))
      (admin-workbench-index-response request)

      (and (= request-method :get) workbench-id)
      (admin-workbench-response request workbench-id)

      (and (= request-method :post) workbench-id)
      (admin-workbench-action-response request workbench-id)

      (and (= request-method :get) game-trick-analysis-id)
      (admin-game-trick-analysis-response request game-trick-analysis-id)

      (and (= request-method :get) game-hand-detail-id)
      (admin-game-hand-detail-response request game-hand-detail-id)

      (and (= request-method :get) game-snapshot-id)
      (admin-game-snapshot-response request game-snapshot-id)

      (and (= request-method :get) game-snapshot-edn-id)
      (admin-game-snapshot-edn-response request game-snapshot-edn-id)

      (and (= request-method :get) room-trick-analysis-id)
      (admin-room-trick-analysis-response request room-trick-analysis-id)

      (and (= request-method :get) room-hand-detail-id)
      (admin-room-hand-detail-response request room-hand-detail-id)

      (and (= request-method :get) snapshot-room-id)
      (admin-room-snapshot-response request snapshot-room-id)

      (and (= request-method :get) snapshot-edn-room-id)
      (admin-room-snapshot-edn-response request snapshot-edn-room-id)

      (and (= request-method :get) (= uri "/karbosh/api/health"))
      (edn-response {:ok true :rooms (count @rooms)})

      (and (= request-method :get) (= uri "/karbosh/api/public-rooms"))
      (public-rooms-response)

      (and (= request-method :get) api-room-route)
      (api-room-data-response api-room-route)

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
    (start-audit!)
    (start-room-sweeper!)
    (start-nrepl-if-enabled!)
    (reset! server (http/run-server #'runtime/current-handler {:ip ip :port port}))
    (println (str "Karbosh server listening on " ip ":" port))))

(defn stop! []
  (stop-room-sweeper!)
  (audit/stop!)
  (stop-nrepl!)
  (when-let [stop @server]
    (stop)
    (reset! server nil)))

(defn -main [& _]
  (start!)
  @(promise))
