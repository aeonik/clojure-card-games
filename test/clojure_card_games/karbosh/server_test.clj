(ns clojure-card-games.karbosh.server-test
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.admin :as admin]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.server :as server]
            [clojure-card-games.karbosh.shared.game :as game]))

(def completed-trick
  [{:player :player1 :card [:K :♥]}
   {:player :player2 :card [:A :♥]}
   {:player :player3 :card [:Q :♥]}
   {:player :player4 :card [:J :♥]}
   {:player :player5 :card [10 :♥]}
   {:player :player6 :card [9 :♥]}])

(deftest bot-turn-delay-test
  (let [players (zipmap game/players (repeat {:team 1 :hand []}))
        room (-> (room/new-room "ABC123" 3)
                 (assoc :game {:phase :trick-playing
                               :players players
                               :history [{:type :play-card
                                          :player :player6
                                          :card [9 :♥]}]
                               :completed-tricks [completed-trick]
                               :current-trick []}))]
    (is (server/completed-trick-state? room))
    (is (= server/trick-complete-delay-ms
           (server/bot-turn-delay-ms room)))
    (is (= server/bot-action-delay-ms
           (server/bot-turn-delay-ms (assoc-in room [:game :current-trick]
                                               [{:player :player1 :card [:K :♥]}]))))))

(deftest admin-basic-auth-test
  (with-redefs [server/admin-user (constantly "admin")
                server/admin-password (constantly "secret")]
    (is (server/admin-authorized?
         {:headers {"authorization" "Basic YWRtaW46c2VjcmV0"}}))
    (is (not (server/admin-authorized?
              {:headers {"authorization" "Basic YWRtaW46d3Jvbmc="}})))
    (is (not (server/admin-authorized?
              {:headers {"authorization" "Basic not-base64"}})))))

(deftest admin-reload-requires-authentication-test
  (with-redefs [server/admin-password (constantly "secret")]
    (let [response (server/handler {:request-method :post
                                    :uri "/karbosh/admin/reload"
                                    :headers {"host" "dc3systems.com"}})]
      (is (= 401 (:status response)))
      (is (= "Authentication required" (:body response))))))

(deftest admin-reload-rejects-cross-origin-post-test
  (with-redefs [server/admin-user (constantly "admin")
                server/admin-password (constantly "secret")
                server/allowed-origins (constantly #{"https://dc3systems.com"})]
    (let [response (server/handler
                    {:request-method :post
                     :uri "/karbosh/admin/reload"
                     :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                               "host" "dc3systems.com"
                               "origin" "https://evil.example"}})]
      (is (= 403 (:status response)))
      (is (= "Forbidden" (:body response))))))

(deftest admin-reload-reloads-without-resetting-rooms-test
  (let [old-rooms @server/rooms]
    (try
      (reset! server/rooms {"ABC123" {:created-at 1 :game {} :connections {}}})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")
                    server/reload-karbosh-namespaces!
                    (fn []
                      {:ok true
                       :reloaded '[clojure-card-games.karbosh.server]
                       :rooms (count @server/rooms)
                       :open-websockets (count @server/open-websockets)})]
        (let [response (server/handler
                        {:request-method :post
                         :uri "/karbosh/admin/reload"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})
              body (edn/read-string (:body response))]
          (is (= 200 (:status response)))
          (is (:ok body))
          (is (= 1 (:rooms body)))
          (is (contains? @server/rooms "ABC123"))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest refresh-room-view-state-adds-legacy-bot-personas-test
  (let [out (async/chan 1)
        old-rooms @server/rooms
        room (-> (room/new-room "ABC123" 9)
                 (room/seat-player :player1 "Human")
                 (assoc-in [:seats :player2]
                           {:name "Bot 2"
                            :connected? true
                            :bot? true})
                 (assoc :connections {:human {:player :player1
                                               :out out}}))]
    (try
      (reset! server/rooms {"ABC123" room})
      (server/refresh-room-view-state!)
      (let [message (async/<!! out)
            bot-view (some #(when (= :player2 (:id %)) %)
                           (get-in message [:view :players]))]
        (is (some? (get-in @server/rooms ["ABC123" :seats :player2 :persona])))
        (is (some? (:persona bot-view)))
        (is (= (:name (:persona bot-view)) (:name bot-view))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest audit-current-rooms-records-live-room-snapshots-test
  (let [old-rooms @server/rooms
        records (atom [])]
    (try
      (reset! server/rooms {"ABC123" (room/new-room "ABC123" 9)
                            "STALE" nil})
      (with-redefs [audit/record-room! (fn [event-type room]
                                         (swap! records conj [event-type (:id room)]))]
        (server/audit-current-rooms! :reload-snapshot)
        (is (= [[:reload-snapshot "ABC123"]] @records)))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-delete-room-removes-room-and-notifies-clients-test
  (let [out (async/chan 2)
        old-rooms @server/rooms
        old-bot-turns @server/bot-turns
        room (room/join-room (room/new-room "ABC123" 9)
                             {:conn-id :first
                              :out out
                              :name "First"})]
    (try
      (reset! server/rooms {"ABC123" room})
      (reset! server/bot-turns {"ABC123" :pending})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :delete
                         :uri "/karbosh/admin/rooms/ABC123"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})
              message (async/<!! out)]
          (is (= 204 (:status response)))
          (is (= :room-closed (:op message)))
          (is (= "ABC123" (:room-id message)))
          (is (not (contains? @server/rooms "ABC123")))
          (is (not (contains? @server/bot-turns "ABC123")))))
      (finally
        (reset! server/rooms old-rooms)
        (reset! server/bot-turns old-bot-turns)))))

(deftest origin-allowlist-test
  (with-redefs [server/allowed-origins (constantly #{"https://dc3systems.com"})]
    (is (server/origin-allowed?
         {:headers {"origin" "https://dc3systems.com"
                    "host" "dc3systems.com"}}))
    (is (server/origin-allowed?
         {:headers {"origin" "https://dc3systems.com:443"
                    "host" "dc3systems.com"}}))
    (is (not (server/origin-allowed?
              {:headers {"origin" "https://evil.example"
                         "host" "dc3systems.com"}})))))

(deftest same-host-origin-default-test
  (with-redefs [server/allowed-origins (constantly #{})]
    (is (server/origin-allowed?
         {:headers {"origin" "https://dc3systems.com"
                    "host" "dc3systems.com"}}))
    (is (server/origin-allowed?
         {:headers {"origin" "https://dc3systems.com:443"
                    "host" "dc3systems.com:443"}}))
    (is (not (server/origin-allowed?
              {:headers {"origin" "https://evil.example"
                         "host" "dc3systems.com"}})))
    (is (not (server/origin-allowed?
              {:headers {"origin" "null"
                         "host" "dc3systems.com"}})))))

(deftest response-security-headers-test
  (let [headers (:headers (server/response 200 "ok"))]
    (is (= "nosniff" (get headers "X-Content-Type-Options")))
    (is (= "no-referrer" (get headers "Referrer-Policy")))
    (is (= "DENY" (get headers "X-Frame-Options")))
    (is (re-find #"frame-ancestors 'none'"
                 (get headers "Content-Security-Policy")))
    (is (re-find #"media-src 'self' data:"
                 (get headers "Content-Security-Policy")))
    (is (re-find #"form-action 'self'"
                 (get headers "Content-Security-Policy")))))

(deftest room-limit-test
  (let [out (async/chan 1)
        old-rooms @server/rooms]
    (try
      (reset! server/rooms {})
      (with-redefs [server/max-rooms (constantly 0)]
        (is (nil? (server/create-room! :conn out {:name "Human"})))
        (is (= {:op :error :message "Room limit reached"}
               (async/<!! out))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest create-room-can-mark-room-public-test
  (let [out (async/chan 1)
        old-rooms @server/rooms]
    (try
      (reset! server/rooms {})
      (with-redefs [server/unique-room-id (constantly "PUB123")]
        (is (= "PUB123" (server/create-room! :conn out {:name "Human"
                                                        :public? true})))
        (is (true? (get-in @server/rooms ["PUB123" :public?]))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest room-connection-limit-test
  (let [out (async/chan 1)
        old-rooms @server/rooms
        state (room/join-room (room/new-room "ABC123" 9)
                              {:conn-id :first
                               :out nil
                               :name "First"})]
    (try
      (reset! server/rooms {"ABC123" state})
      (with-redefs [server/max-room-connections (constantly 1)]
        (server/join-room! :second out {:room-id "ABC123"
                                        :name "Second"})
        (is (= {:op :error :message "Room connection limit reached"}
               (async/<!! out))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest room-preview-test
  (let [old-rooms @server/rooms
        room (-> (room/new-room "ABC123" 9)
                 (room/seat-player :player1 "Dave")
                 (room/seat-bot :player2))]
    (try
      (reset! server/rooms {"ABC123" room})
      (let [response (server/handler {:request-method :get
                                      :uri "/karbosh/api/room/abc123"})
            body (edn/read-string (:body response))
            player1 (first (:players body))
            player2 (second (:players body))
            player3 (nth (:players body) 2)]
        (is (= 200 (:status response)))
        (is (:ok body))
        (is (= "ABC123" (:room-id body)))
        (is (false? (:public? body)))
        (is (= "Dave" (:name player1)))
        (is (false? (:open? player1)))
        (is (false? (:joinable? player1)))
        (is (true? (:bot? player2)))
        (is (true? (:joinable? player2)))
        (is (true? (:open? player3)))
        (is (true? (:joinable? player3)))
        (is (not (contains? player1 :hand))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-room-snapshot-test
  (let [old-rooms @server/rooms
        room (-> (room/new-room "ABC123" 9)
                 (room/seat-player :player1 "Dave")
                 (assoc-in [:game :phase] :trick-playing)
                 (assoc-in [:game :trump] :♠)
                 (assoc-in [:game :completed-tricks] [completed-trick])
                 (assoc-in [:game :history]
                           (mapv #(assoc % :type :play-card) completed-trick))
                 (assoc-in [:game :tricks-this-hand] {1 0 2 1})
                 (assoc-in [:game :current-player] :player2)
                 (assoc :connections {:conn {:player :player1
                                              :out :channel}}))]
    (try
      (reset! server/rooms {"ABC123" room})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :get
                         :uri "/karbosh/admin/rooms/ABC123/snapshot"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})]
          (is (= 200 (:status response)))
          (is (re-find #"text/html" (get-in response [:headers "Content-Type"])))
          (is (re-find #"Room ABC123 History" (:body response)))
          (is (re-find #"Play by Play" (:body response)))
          (is (re-find #"Starting Hands" (:body response)))
          (is (re-find #"starting-hands-strip" (:body response)))
          (is (re-find #"starting-hand-row" (:body response)))
          (is (re-find #"Trick 1" (:body response)))
          (is (re-find #"Winner: player2" (:body response)))
          (is (re-find #"\.trick \.card" (:body response)))
          (is (re-find #"\.compact-list \.card" (:body response)))
          (is (re-find #"\.starting-hands-strip \.card" (:body response)))
          (is (re-find #"Raw EDN" (:body response)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-room-snapshot-edn-test
  (let [old-rooms @server/rooms
        room (-> (room/new-room "ABC123" 9)
                 (room/seat-player :player1 "Dave")
                 (assoc :connections {:conn {:player :player1
                                              :out :channel}}))]
    (try
      (reset! server/rooms {"ABC123" room})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :get
                         :uri "/karbosh/admin/rooms/ABC123/snapshot.edn"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})
              body (edn/read-string (:body response))]
          (is (= 200 (:status response)))
          (is (:ok body))
          (is (= "ABC123" (:room-id body)))
          (is (= 9 (get-in body [:room :seed])))
          (is (= 9 (get-in body [:room :game :initial-seed])))
          (is (not (contains? (:room body) :connections)))
          (is (= "Dave" (get-in body [:room :seats :player1 :name])))
          (is (seq (get-in body [:view :debug :deals])))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest public-rooms-response-test
  (let [old-rooms @server/rooms
        public-room (-> (room/new-room "PUB123" 9 true)
                        (room/seat-player :player1 "Dave")
                        (room/seat-bot :player2))
        private-room (room/new-room "PRIVATE" 10 false)]
    (try
      (reset! server/rooms {"PUB123" public-room
                            "PRIVATE" private-room})
      (let [response (server/handler {:request-method :get
                                      :uri "/karbosh/api/public-rooms"})
            body (edn/read-string (:body response))
            rooms (:rooms body)]
        (is (= 200 (:status response)))
        (is (:ok body))
        (is (= ["PUB123"] (mapv :room-id rooms)))
        (is (= 1 (:player-count (first rooms))))
        (is (= 5 (:available-count (first rooms))))
        (is (not (contains? (first rooms) :players)))
        (is (not (contains? (first rooms) :hands))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest set-room-visibility-broadcasts-public-flag-test
  (let [out (async/chan 2)
        old-rooms @server/rooms
        state (room/join-room (room/new-room "ABC123" 9)
                              {:conn-id :human
                               :out out
                               :name "Human"})]
    (try
      (reset! server/rooms {"ABC123" state})
      (server/set-room-visibility! "ABC123" out true)
      (let [message (async/<!! out)]
        (is (true? (get-in @server/rooms ["ABC123" :public?])))
        (is (= :state (:op message)))
        (is (true? (get-in message [:view :public?]))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest missing-room-preview-test
  (let [old-rooms @server/rooms]
    (try
      (reset! server/rooms {})
      (let [response (server/handler {:request-method :get
                                      :uri "/karbosh/api/room/missing"})
            body (edn/read-string (:body response))]
        (is (= 404 (:status response)))
        (is (false? (:ok body)))
        (is (= "MISSING" (:room-id body))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest missing-room-update-does-not-create-stale-entry-test
  (let [old-rooms @server/rooms]
    (try
      (reset! server/rooms {})
      (is (nil? (server/update-room! "MISSING" identity)))
      (is (= {} @server/rooms))
      (reset! server/rooms {"STALE" nil})
      (is (nil? (server/update-room! "STALE" identity)))
      (is (= {} @server/rooms))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest idle-room-ids-test
  (with-redefs [server/idle-room-ms (constantly 1000)]
    (is (= #{"OLD"}
           (set (server/idle-room-ids
                 {"ACTIVE" {:created-at 0
                            :connections {:conn {}}
                            :game {}}
                  "NEW" {:created-at 1200
                         :connections {}
                         :game {}}
                  "OLD" {:created-at 0
                         :empty-since 500
                         :connections {}
                         :game {}}}
                 1500))))))

(deftest leave-room-removes-connection-test
  (let [out (async/chan 1)
        old-rooms @server/rooms
        room (room/join-room (room/new-room "ABC123" 9)
                             {:conn-id :human
                              :out out
                              :name "Human"})]
    (try
      (reset! server/rooms {"ABC123" room})
      (server/leave-room! :human out "ABC123")
      (let [message (async/<!! out)
            room (get @server/rooms "ABC123")]
        (is (= :left-room (:op message)))
        (is (empty? (:connections room)))
        (is (:empty-since room))
        (is (false? (get-in room [:seats :player1 :connected?]))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest kick-player-removes-seat-and-notifies-clients-test
  (let [owner-out (async/chan 2)
        kicked-out (async/chan 2)
        old-rooms @server/rooms
        room (-> (room/new-room "ABC123" 9)
                 (room/join-room {:conn-id :owner
                                  :out owner-out
                                  :player :player1
                                  :name "Owner"})
                 (room/join-room {:conn-id :guest
                                  :out kicked-out
                                  :player :player2
                                  :name "Guest"})
                 (room/ensure-owner))]
    (try
      (reset! server/rooms {"ABC123" room})
      (is (true? (server/kick-player! :owner "ABC123" owner-out :player2)))
      (let [state-message (async/<!! owner-out)
            kicked-message (async/<!! kicked-out)
            room (get @server/rooms "ABC123")]
        (is (= :state (:op state-message)))
        (is (= :kicked (:op kicked-message)))
        (is (= :player2 (:player kicked-message)))
        (is (not (contains? (:seats room) :player2)))
        (is (not (contains? (:connections room) :guest)))
        (is (contains? (:connections room) :owner)))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest non-owner-cannot-kick-player-test
  (let [guest-out (async/chan 1)
        old-rooms @server/rooms
        room (-> (room/new-room "ABC123" 9)
                 (room/join-room {:conn-id :owner
                                  :out nil
                                  :player :player1
                                  :name "Owner"})
                 (room/join-room {:conn-id :guest
                                  :out guest-out
                                  :player :player2
                                  :name "Guest"})
                 (room/ensure-owner))]
    (try
      (reset! server/rooms {"ABC123" room})
      (server/kick-player! :guest "ABC123" guest-out :player1)
      (let [message (async/<!! guest-out)
            room (get @server/rooms "ABC123")]
        (is (= :error (:op message)))
        (is (= "Only the room owner can kick players" (:message message)))
        (is (contains? (:seats room) :player1))
        (is (contains? (:connections room) :owner)))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-dashboard-ignores-stale-room-entries-test
  (let [html (admin/render-dashboard
              {:rooms {"STALE" nil}
               :metrics {:started-at 1000}
               :pending-bot-count 0
               :open-websocket-count 0
               :limits {:max-rooms 128
                        :max-room-connections 24
                        :max-websocket-connections 256
                        :max-message-bytes 8192}
               :started-at 1000})]
    (is (string? html))
    (is (re-find #"No rooms are currently running" html))))

(deftest admin-dashboard-main-html-renders-websocket-payload-test
  (let [html (admin/render-dashboard-main-html
              {:rooms {"ABC123" (room/new-room "ABC123" 9)}
               :metrics {:started-at 1000}
               :pending-bot-count 0
               :open-websocket-count 0
               :limits {:max-rooms 128
                        :max-room-connections 24
                        :max-websocket-connections 256
                        :max-message-bytes 8192
                        :idle-room-ms 14400000}
               :started-at 1000})]
    (is (string? html))
    (is (re-find #"<main id=\"admin-main\">" html))
    (is (re-find #"data-delete-room=\"ABC123\"" html))))

(deftest admin-dashboard-renders-delete-room-form-test
  (let [html (admin/render-dashboard
              {:rooms {"ABC123" (room/new-room "ABC123" 9)}
               :metrics {:started-at 1000}
               :pending-bot-count 0
               :open-websocket-count 0
               :limits {:max-rooms 128
                        :max-room-connections 24
                        :max-websocket-connections 256
                        :max-message-bytes 8192
                        :idle-room-ms 14400000}
               :started-at 1000})]
    (is (not (re-find #"admin/delete-room\?room=" html)))
    (is (re-find #"data-delete-room=\"ABC123\"" html))
    (is (re-find #"href=\"/karbosh/admin/rooms/ABC123/snapshot\"" html))
    (is (re-find #"src=\"/karbosh/assets/js/admin.js\?v=20260604-stream\"" html))
    (is (re-find #"\.trick>div" html))
    (is (re-find #"\.trick \.card" html))
    (is (re-find #"\.compact-list \.card" html))
    (is (not (re-find #"\.trick&gt;div" html)))
    (is (re-find #">Delete</button>" html))))

(deftest websocket-limit-test
  (let [old-websockets @server/open-websockets]
    (try
      (with-redefs [server/max-websocket-connections (constantly 1)]
        (reset! server/open-websockets #{:existing})
        (is (server/websocket-limit-reached?))
        (reset! server/open-websockets #{})
        (is (not (server/websocket-limit-reached?))))
      (finally
        (reset! server/open-websockets old-websockets)))))

(deftest oversized-message-test
  (let [out (async/chan 1)]
    (with-redefs [server/max-message-bytes (constantly 4)]
      (server/handle-raw-message! :conn out (atom nil) "{:op :ping}")
      (is (= {:op :error :message "Message is too large"}
             (async/<!! out))))))
