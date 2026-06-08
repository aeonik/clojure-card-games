(ns clojure-card-games.karbosh.server-test
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure-card-games.karbosh.admin :as admin]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.server :as server]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.storage :as storage])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def completed-trick
  [{:player :player1 :card [:K :♥]}
   {:player :player2 :card [:A :♥]}
   {:player :player3 :card [:Q :♥]}
   {:player :player4 :card [:J :♥]}
   {:player :player5 :card [10 :♥]}
   {:player :player6 :card [9 :♥]}])

(defn completed-room [room-id seed]
  (-> (room/new-room room-id seed)
      (room/seat-player :player1 "Dave")
      (assoc-in [:game :phase] :game-over)
      (assoc-in [:game :winner] 1)
      (assoc-in [:game :scores] {1 52 2 10})
      (assoc-in [:game :hand-history]
                [{:hand-index 0
                  :bid {:type :bid
                        :player :player1
                        :bid-type :bid
                        :value 4}
                  :trump :♠
                  :tricks {1 4 2 4}
                  :points {1 4 2 0}
                  :scores-after {1 4 2 0}
                  :history [{:type :bid
                             :player :player1
                             :bid-type :bid
                             :value 4}]
                  :completed-tricks [completed-trick]}])))

(defn bot-persona [name]
  (some #(when (= name (:name %)) %)
        room/bot-personas))

(defn completed-room-with-bots [room-id seed]
  (-> (completed-room room-id seed)
      (room/seat-bot :player2 (bot-persona "Trumpelstiltskin"))
      (room/seat-bot :player3 (bot-persona "Deal-E"))))

(defn played-room [room]
  (assoc-in room [:game :hand-history] [{:hand-index 0}]))

(defn connected-room [room]
  (room/add-connection room :conn :player1 nil))

(use-fixtures
  :each
  (fn [test]
    (let [room-dir (.toFile (Files/createTempDirectory "karbosh-rooms-test"
                                                       (make-array FileAttribute 0)))
          audit-dir (.toFile (Files/createTempDirectory "karbosh-audit-server-test"
                                                        (make-array FileAttribute 0)))]
      (with-redefs [server/room-dir (constantly (.getPath room-dir))
                    server/audit-dir (constantly (.getPath audit-dir))]
        (test)))))

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
    (is (= server/fast-trick-complete-delay-ms
           (server/bot-turn-delay-ms (assoc room :fast-mode? true))))
    (is (= server/bot-action-delay-ms
           (server/bot-turn-delay-ms (assoc-in room [:game :current-trick]
                                               [{:player :player1 :card [:K :♥]}]))))
    (is (= server/fast-bot-action-delay-ms
           (server/bot-turn-delay-ms (-> room
                                         (assoc :fast-mode? true)
                                         (assoc-in [:game :current-trick]
                                                   [{:player :player1 :card [:K :♥]}])))))))

(deftest reloadable-namespaces-order-test
  (let [namespaces (vec server/reloadable-namespaces)
        analysis-index (.indexOf namespaces 'clojure-card-games.karbosh.analysis)
        bot-index (.indexOf namespaces 'clojure-card-games.karbosh.bot)
        room-index (.indexOf namespaces 'clojure-card-games.karbosh.room)]
    (is (not= -1 analysis-index))
    (is (not= -1 bot-index))
    (is (not= -1 room-index))
    (is (< analysis-index bot-index))
    (is (< bot-index room-index))))

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
      (reset! server/rooms {"ABC123" (played-room (room/new-room "ABC123" 9))
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

(deftest create-room-can-set-room-options-test
  (let [out (async/chan 1)
        old-rooms @server/rooms]
    (try
      (reset! server/rooms {})
      (with-redefs [server/unique-room-id (constantly "PUB123")]
        (is (= "PUB123" (server/create-room! :conn out {:name "Human"
                                                        :public? true
                                                        :fast-mode? true
                                                        :seed -42})))
        (is (true? (get-in @server/rooms ["PUB123" :public?])))
        (is (true? (get-in @server/rooms ["PUB123" :fast-mode?])))
        (is (= -42 (get-in @server/rooms ["PUB123" :seed])))
        (is (= -42 (get-in @server/rooms ["PUB123" :game :initial-seed]))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest create-room-can-parse-string-seed-test
  (let [out (async/chan 1)
        old-rooms @server/rooms]
    (try
      (reset! server/rooms {})
      (with-redefs [server/unique-room-id (constantly "SEED42")]
        (is (= "SEED42" (server/create-room! :conn out {:name "Human"
                                                        :seed "42"})))
        (is (= 42 (get-in @server/rooms ["SEED42" :seed])))
        (is (= 42 (get-in @server/rooms ["SEED42" :game :initial-seed]))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest create-room-rejects-invalid-seed-test
  (let [out (async/chan 1)
        old-rooms @server/rooms]
    (try
      (reset! server/rooms {})
      (is (nil? (server/create-room! :conn out {:name "Human"
                                                :seed "not-a-seed"})))
      (is (= {:op :error :message "Seed must be an integer"}
             (async/<!! out)))
      (is (empty? @server/rooms))
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
        ai {:policy :hybrid-ruff-invite
            :engine :probability-ruff-invite
            :reason :lead-safe-card
            :selected {:card [:K :♥]
                       :score 400
                       :risk 0.25
                       :hypergeom {:prob-can-beat 0.25
                                   :prob-any-higher 0.4
                                   :prob-higher-follow 0.2
                                   :higher-unseen 2
                                   :higher-follow-unseen 1
                                   :higher-trump-unseen 0}}
            :candidates [{:card [:A :♥]
                          :score 500
                          :risk 0.05
                          :good? true
                          :winning? true}]}
        trick (update completed-trick 0 assoc :ai ai)
        room (-> (room/new-room "ABC123" 9)
                 (room/seat-player :player1 "Dave")
                 (assoc-in [:game :phase] :trick-playing)
                 (assoc-in [:game :trump] :♥)
                 (assoc-in [:game :initial-hands :player1]
                           [[:A :♣] [9 :♣] [:J :♦] [:K :♠]
                            [10 :♥] [:A :♥] [9 :♦] [:Q :♣]])
                 (assoc-in [:game :completed-tricks] [trick])
                 (assoc-in [:game :history]
                           (mapv #(assoc % :type :play-card) trick))
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
                                   "host" "dc3systems.com"}})
              detail-response (server/handler
                               {:request-method :get
                                :uri "/karbosh/admin/rooms/ABC123/snapshot/hands/0"
                                :headers {"host" "dc3systems.com"}})]
          (is (= 200 (:status response)))
          (is (re-find #"text/html" (get-in response [:headers "Content-Type"])))
          (is (re-find #"Room ABC123 History" (:body response)))
          (is (re-find #"Hand history" (:body response)))
          (is (re-find #"Starting hands" (:body response)))
          (is (re-find #"starting-hands-strip" (:body response)))
          (is (< (.indexOf (:body response) "J♦")
                 (.indexOf (:body response) "A♣")))
          (is (re-find #"Explain" (:body response)))
          (is (re-find #"href=\"/karbosh/admin/rooms/ABC123/snapshot/hands/0\""
                       (:body response)))
          (is (re-find #"href=\"/karbosh/admin/rooms/ABC123/snapshot/hands/0#trick-1\""
                       (:body response)))
          (is (re-find #"T1" (:body response)))
          (is (re-find #"class=\"suit heart\">♥" (:body response)))
          (is (re-find #"\.suit\.heart,\.suit\.diamond" (:body response)))
          (is (re-find #"Raw EDN" (:body response)))
          (is (= 200 (:status detail-response)))
          (is (re-find #"Karbosh hand detail" (:body detail-response)))
          (is (re-find #"Play by Play" (:body detail-response)))
          (is (re-find #"AI Policies" (:body detail-response)))
          (is (re-find #"AI Decisions" (:body detail-response)))
          (is (re-find #"Hybrid ruff invite" (:body detail-response)))
          (is (re-find #"Lead safe card" (:body detail-response)))
          (is (re-find #"P beat" (:body detail-response)))
          (is (re-find #"Can be beaten" (:body detail-response)))
          (is (re-find #"Candidate cards" (:body detail-response)))
          (is (re-find #"25.0%" (:body detail-response)))
          (is (re-find #"5.0%" (:body detail-response)))
          (is (re-find #"Starting Hands" (:body detail-response)))
          (is (re-find #"starting-hands-strip" (:body detail-response)))
          (is (re-find #"starting-hand-row" (:body detail-response)))
          (is (re-find #"Trick 1" (:body detail-response)))
          (is (re-find #"id=\"trick-1\"" (:body detail-response)))
          (is (re-find #"Winner: player4" (:body detail-response)))))
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
                         :headers {"host" "dc3systems.com"}})
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

(deftest historical-snapshot-html-is-public-but-admin-remains-protected-test
  (let [old-rooms @server/rooms
        dir (.toFile (Files/createTempDirectory "karbosh-share-history-test"
                                                (make-array FileAttribute 0)))
        historical-room (assoc (completed-room "OLD123" 17)
                               :game-started-at 111)]
    (try
      (audit/append-record! dir (audit/room-record :room-delete-idle
                                                   historical-room
                                                   1000))
      (reset! server/rooms {})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")
                    server/audit-dir (constantly (.getPath dir))]
        (let [dashboard-response (server/handler
                                  {:request-method :get
                                   :uri "/karbosh/admin"
                                   :headers {"host" "dc3systems.com"}})
              history-response (server/handler
                                {:request-method :get
                                 :uri "/karbosh/admin/history"
                                 :headers {"host" "dc3systems.com"}})
              room-html-response (server/handler
                                  {:request-method :get
                                   :uri "/karbosh/admin/rooms/OLD123/snapshot"
                                   :headers {"host" "dc3systems.com"}})
              room-edn-response (server/handler
                                 {:request-method :get
                                  :uri "/karbosh/admin/rooms/OLD123/snapshot.edn"
                                  :headers {"host" "dc3systems.com"}})
              game-html-response (server/handler
                                  {:request-method :get
                                   :uri "/karbosh/admin/history/OLD123/17/111/snapshot"
                                   :headers {"host" "dc3systems.com"}})
              game-edn-response (server/handler
                                 {:request-method :get
                                  :uri "/karbosh/admin/history/OLD123/17/111/snapshot.edn"
                                  :headers {"host" "dc3systems.com"}})
              game-hand-response (server/handler
                                  {:request-method :get
                                   :uri "/karbosh/admin/history/OLD123/17/111/snapshot/hands/0"
                                   :headers {"host" "dc3systems.com"}})
              room-edn-body (edn/read-string (:body room-edn-response))
              game-edn-body (edn/read-string (:body game-edn-response))]
          (is (= 401 (:status dashboard-response)))
          (is (= 401 (:status history-response)))
          (is (= 200 (:status room-html-response)))
          (is (re-find #"Room OLD123 History" (:body room-html-response)))
          (is (= 200 (:status room-edn-response)))
          (is (:ok room-edn-body))
          (is (= "OLD123" (:room-id room-edn-body)))
          (is (= 200 (:status game-html-response)))
          (is (re-find #"Room OLD123 History" (:body game-html-response)))
          (is (= 200 (:status game-edn-response)))
          (is (:ok game-edn-body))
          (is (= 200 (:status game-hand-response)))
          (is (re-find #"Karbosh hand detail" (:body game-hand-response)))
          (is (= "OLD123" (:room-id game-edn-body)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest live-snapshot-html-direct-links-are-public-test
  (let [old-rooms @server/rooms
        room (room/new-room "ABC123" 9)
        timestamp (:game-started-at room)]
    (try
      (reset! server/rooms {"ABC123" room})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [room-response (server/handler
                             {:request-method :get
                              :uri "/karbosh/admin/rooms/ABC123/snapshot"
                              :headers {"host" "dc3systems.com"}})
              game-response (server/handler
                             {:request-method :get
                              :uri (str "/karbosh/admin/history/ABC123/9/"
                                        timestamp
                                        "/snapshot")
                              :headers {"host" "dc3systems.com"}})
              room-hand-response (server/handler
                                  {:request-method :get
                                   :uri "/karbosh/admin/rooms/ABC123/snapshot/hands/0"
                                   :headers {"host" "dc3systems.com"}})
              game-hand-response (server/handler
                                  {:request-method :get
                                   :uri (str "/karbosh/admin/history/ABC123/9/"
                                             timestamp
                                             "/snapshot/hands/0")
                                   :headers {"host" "dc3systems.com"}})]
          (is (= 200 (:status room-response)))
          (is (re-find #"Room ABC123 History" (:body room-response)))
          (is (= 200 (:status game-response)))
          (is (re-find #"Room ABC123 History" (:body game-response)))
          (is (= 200 (:status room-hand-response)))
          (is (re-find #"Karbosh hand detail" (:body room-hand-response)))
          (is (= 200 (:status game-hand-response)))
          (is (re-find #"Karbosh hand detail" (:body game-hand-response)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-dashboard-renders-historical-rooms-test
  (let [old-rooms @server/rooms
        dir (.toFile (Files/createTempDirectory "karbosh-history-test"
                                                (make-array FileAttribute 0)))
        historical-room (completed-room "OLD123" 17)]
    (try
      (audit/append-record! dir (audit/room-record :room-delete-idle
                                                   historical-room
                                                   1000))
      (reset! server/rooms {})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")
                    server/audit-dir (constantly (.getPath dir))]
        (let [response (server/handler
                        {:request-method :get
                         :uri "/karbosh/admin"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})]
          (is (= 200 (:status response)))
          (is (re-find #"No rooms are currently active" (:body response)))
          (is (re-find #"Historical rooms" (:body response)))
          (is (re-find #"OLD123" (:body response)))
          (is (re-find #"Bid trends" (:body response)))
          (is (re-find #"href=\"/karbosh/admin/history\"" (:body response)))
          (is (re-find #"href=\"/karbosh/admin/rooms/OLD123/snapshot\"" (:body response)))
          (is (re-find #"href=\"/karbosh/admin/rooms/OLD123/snapshot.edn\"" (:body response)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-history-renders-all-games-test
  (let [old-rooms @server/rooms
        dir (.toFile (Files/createTempDirectory "karbosh-full-history-test"
                                                (make-array FileAttribute 0)))
        first-game (assoc (completed-room-with-bots "ROOM1" 17)
                          :game-started-at 111)
        replayed-game (assoc (completed-room-with-bots "ROOM1" 17)
                             :game-started-at 222)
        second-game (assoc (completed-room-with-bots "ROOM1" 99)
                           :game-started-at 333)]
    (try
      (audit/append-record! dir (audit/room-record :room-delete-idle
                                                   first-game
                                                   1000))
      (audit/append-record! dir (audit/room-record :room-delete-idle
                                                   replayed-game
                                                   1500))
      (audit/append-record! dir (audit/room-record :room-delete-idle
                                                   second-game
                                                   2000))
      (reset! server/rooms {})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")
                    server/audit-dir (constantly (.getPath dir))]
        (let [response (server/handler
                        {:request-method :get
                         :uri "/karbosh/admin/history"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})]
          (is (= 200 (:status response)))
          (is (re-find #"Game history" (:body response)))
          (is (re-find #"All games" (:body response)))
          (is (re-find #"ROOM1" (:body response)))
          (is (re-find #">17<" (:body response)))
          (is (re-find #">99<" (:body response)))
          (is (re-find #"Bot strategies" (:body response)))
          (is (re-find #"Hybrid preservation" (:body response)))
          (is (re-find #"Hybrid ruff invite" (:body response)))
          (is (re-find #"Hybrid defender exit" (:body response)))
          (is (re-find #"Probability ruff invite" (:body response)))
          (is (re-find #"Hybrid" (:body response)))
          (is (re-find #"Bot personas" (:body response)))
          (is (re-find #"Trumpelstiltskin" (:body response)))
          (is (re-find #"Deal-E" (:body response)))
          (is (re-find #"href=\"/karbosh/admin/history/ROOM1/17/111/snapshot\"" (:body response)))
          (is (re-find #"href=\"/karbosh/admin/history/ROOM1/17/222/snapshot.edn\"" (:body response)))
          (is (re-find #"href=\"/karbosh/admin/history/ROOM1/99/333/snapshot.edn\"" (:body response)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-history-snapshot-loads-specific-game-seed-test
  (let [old-rooms @server/rooms
        dir (.toFile (Files/createTempDirectory "karbosh-game-snapshot-test"
                                                (make-array FileAttribute 0)))
        first-game (assoc (completed-room "ROOM1" 17) :game-started-at 111)
        replayed-game (-> (completed-room "ROOM1" 17)
                          (assoc :game-started-at 222)
                          (assoc-in [:game :winner] 2)
                          (assoc-in [:game :scores] {1 10 2 52}))]
    (try
      (audit/append-record! dir (audit/room-record :room-delete-idle
                                                   first-game
                                                   1000))
      (audit/append-record! dir (audit/room-record :room-delete-idle
                                                   replayed-game
                                                   2000))
      (reset! server/rooms {})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")
                    server/audit-dir (constantly (.getPath dir))]
        (let [response (server/handler
                        {:request-method :get
                         :uri "/karbosh/admin/history/ROOM1/17/222/snapshot.edn"
                         :headers {"host" "dc3systems.com"}})
              body (edn/read-string (:body response))]
          (is (= 200 (:status response)))
          (is (= 17 (:seed body)))
          (is (= 222 (:timestamp body)))
          (is (= 17 (get-in body [:room :seed])))
          (is (= 17 (get-in body [:room :game :initial-seed])))
          (is (= 52 (get-in body [:room :game :scores 2])))
          (is (= "ROOM1" (:room-id body)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-room-snapshot-falls-back-to-audit-test
  (let [old-rooms @server/rooms
        dir (.toFile (Files/createTempDirectory "karbosh-snapshot-history-test"
                                                (make-array FileAttribute 0)))
        historical-room (completed-room "OLD123" 17)]
    (try
      (audit/append-record! dir (audit/room-record :room-delete-idle
                                                   historical-room
                                                   1000))
      (reset! server/rooms {})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")
                    server/audit-dir (constantly (.getPath dir))]
        (let [html-response (server/handler
                             {:request-method :get
                              :uri "/karbosh/admin/rooms/OLD123/snapshot"
                              :headers {"host" "dc3systems.com"}})
              edn-response (server/handler
                            {:request-method :get
                             :uri "/karbosh/admin/rooms/OLD123/snapshot.edn"
                             :headers {"host" "dc3systems.com"}})
              body (edn/read-string (:body edn-response))]
          (is (= 200 (:status html-response)))
          (is (re-find #"Room OLD123 History" (:body html-response)))
          (is (re-find #"Hand history" (:body html-response)))
          (is (re-find #"Starting hands" (:body html-response)))
          (is (re-find #"href=\"/karbosh/admin/rooms/OLD123/snapshot/hands/0\""
                       (:body html-response)))
          (is (= 200 (:status edn-response)))
          (is (:ok body))
          (is (:historical? body))
          (is (= "OLD123" (:room-id body)))
          (is (= :room-delete-idle (get-in body [:record :type])))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest public-rooms-response-test
  (let [old-rooms @server/rooms
        public-room (-> (room/new-room "PUB123" 9 true)
                        (room/seat-player :player1 "Dave")
                        (room/seat-bot :player2)
                        connected-room)
        idle-public-room (-> (room/new-room "IDLE12" 11 true)
                             (room/seat-player :player1 "Idle"))
        private-room (-> (room/new-room "PRIVATE" 10 false)
                         (room/seat-player :player1 "Private")
                         connected-room)]
    (try
      (reset! server/rooms {"PUB123" public-room
                            "IDLE12" idle-public-room
                            "PRIVATE" private-room})
      (let [response (server/handler {:request-method :get
                                      :uri "/karbosh/api/public-rooms"})
            body (edn/read-string (:body response))
            rooms (:rooms body)]
        (is (= 200 (:status response)))
        (is (:ok body))
        (is (= ["PUB123"] (mapv :room-id rooms)))
        (is (= 1 (:player-count (first rooms))))
        (is (= 1 (:connected-count (first rooms))))
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

(deftest set-fast-mode-broadcasts-room-speed-test
  (let [out (async/chan 2)
        old-rooms @server/rooms
        state (room/join-room (room/new-room "ABC123" 9)
                              {:conn-id :human
                               :out out
                               :name "Human"})]
    (try
      (reset! server/rooms {"ABC123" state})
      (server/set-fast-mode! "ABC123" out true)
      (let [message (async/<!! out)]
        (is (true? (get-in @server/rooms ["ABC123" :fast-mode?])))
        (is (= :state (:op message)))
        (is (true? (get-in message [:view :fast-mode?]))))
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

(deftest publish-room-persists-without-audit-append-test
  (let [published (atom [])
        room (room/new-room "ABC123" 9)]
    (with-redefs [audit/record-room! (fn [event-type room]
                                       (swap! published conj [event-type (:id room)])
                                       true)]
      (server/publish-room! "ABC123" room)
      (is (= [] @published))
      (is (storage/room-exists? (server/room-dir) "ABC123"))
      (server/publish-room! "ABC123" (played-room room))
      (is (= [] @published)))))

(deftest save-current-rooms-persists-loaded-rooms-test
  (let [old-rooms @server/rooms]
    (try
      (reset! server/rooms {"ABC123" (room/new-room "ABC123" 9)
                            "STALE" nil})
      (server/save-current-rooms!)
      (is (storage/room-exists? (server/room-dir) "ABC123"))
      (is (not (storage/room-exists? (server/room-dir) "STALE")))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest idle-delete-prunes-zero-hand-archive-test
  (let [old-rooms @server/rooms
        dir (.toFile (Files/createTempDirectory "karbosh-idle-prune-test"
                                                (make-array FileAttribute 0)))
        room (-> (room/new-room "EMPTY1" 9)
                 (assoc :empty-since 0
                        :connections {}))
        file (java.io.File. dir "EMPTY1.edn")]
    (try
      (audit/append-record! dir (audit/room-record :room-publish room 1000))
      (storage/write-room! (server/room-dir) room)
      (reset! server/rooms {"EMPTY1" room})
      (with-redefs [server/audit-dir (constantly (.getPath dir))]
        (is (= room (server/unload-room! "EMPTY1" :idle)))
        (is (not (contains? @server/rooms "EMPTY1")))
        (is (not (.exists file)))
        (is (not (storage/room-exists? (server/room-dir) "EMPTY1"))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest idle-sweep-unloads-played-room-without-closing-it-test
  (let [old-rooms @server/rooms
        room (-> (played-room (room/new-room "PLAYED" 9))
                 (assoc :empty-since 0
                        :connections {}))]
    (try
      (reset! server/rooms {"PLAYED" room})
      (with-redefs [server/idle-room-ms (constantly 1000)]
        (is (= ["PLAYED"] (server/close-idle-rooms!)))
        (is (not (contains? @server/rooms "PLAYED")))
        (is (storage/room-exists? (server/room-dir) "PLAYED"))
        (is (= "PLAYED" (:id (storage/read-room (server/room-dir) "PLAYED")))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest join-room-loads-durable-room-test
  (let [out (async/chan 2)
        old-rooms @server/rooms
        durable (-> (played-room (room/new-room "SAVED1" 9))
                    (room/seat-player :player1 "Dave"))]
    (try
      (storage/write-room! (server/room-dir) durable)
      (reset! server/rooms {})
      (is (true? (server/join-room! :conn out {:room-id "SAVED1"
                                               :name "Dave"
                                               :player "player1"})))
      (is (contains? @server/rooms "SAVED1"))
      (is (= :state (:op (async/<!! out))))
      (is (= :player1 (get-in @server/rooms ["SAVED1" :connections :conn :player])))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest join-room-loads-legacy-audit-room-test
  (let [out (async/chan 2)
        old-rooms @server/rooms
        legacy (-> (played-room (room/new-room "OLD123" 9))
                   (room/seat-player :player1 "Dave"))]
    (try
      (audit/append-record! (server/audit-dir)
                            (audit/room-record :room-delete-idle legacy 1000))
      (reset! server/rooms {})
      (is (true? (server/join-room! :conn out {:room-id "OLD123"
                                               :name "Dave"
                                               :player "player1"})))
      (is (contains? @server/rooms "OLD123"))
      (is (= :state (:op (async/<!! out))))
      (is (storage/room-exists? (server/room-dir) "OLD123"))
      (finally
        (reset! server/rooms old-rooms)))))

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
              {:rooms {"STALE" nil
                       "IDLE12" (room/new-room "IDLE12" 9)}
               :metrics {:started-at 1000}
               :pending-bot-count 0
               :open-websocket-count 0
               :limits {:max-rooms 128
                        :max-room-connections 24
                        :max-websocket-connections 256
                        :max-message-bytes 8192}
               :started-at 1000})]
    (is (string? html))
    (is (re-find #"No rooms are currently active" html))
    (is (not (re-find #"data-delete-room=\"IDLE12\"" html)))
    (is (re-find #"Active rooms" html))
    (is (re-find #"Loaded room cache" html))
    (is (re-find #"Idle unload" html))))

(deftest admin-dashboard-main-html-renders-websocket-payload-test
  (let [html (admin/render-dashboard-main-html
              {:rooms {"ABC123" (connected-room (room/new-room "ABC123" 9))}
               :metrics {:started-at 1000}
               :pending-bot-count 0
               :open-websocket-count 0
               :limits {:max-rooms 128
                        :max-room-connections 24
                        :max-websocket-connections 256
                        :max-message-bytes 8192
                        :idle-room-ms 300000}
               :started-at 1000})]
    (is (string? html))
    (is (re-find #"<main id=\"admin-main\">" html))
    (is (re-find #"data-delete-room=\"ABC123\"" html))))

(deftest admin-dashboard-stream-html-skips-archive-scan-test
  (with-redefs [server/historical-room-records
                (fn []
                  (throw (ex-info "archive scan should not run for stream"
                                  {})))]
    (let [html (server/admin-dashboard-stream-html {:query-string ""})]
      (is (string? html))
      (is (re-find #"Historical archive is available" html))
      (is (not (re-find #"Bid trends" html))))))

(deftest admin-dashboard-renders-delete-room-form-test
  (let [html (admin/render-dashboard
              {:rooms {"ABC123" (connected-room (room/new-room "ABC123" 9))}
               :metrics {:started-at 1000}
               :pending-bot-count 0
               :open-websocket-count 0
               :limits {:max-rooms 128
                        :max-room-connections 24
                        :max-websocket-connections 256
                        :max-message-bytes 8192
                        :idle-room-ms 300000}
               :started-at 1000})]
    (is (not (re-find #"admin/delete-room\?room=" html)))
    (is (re-find #"data-delete-room=\"ABC123\"" html))
    (is (re-find #"href=\"/karbosh/admin/rooms/ABC123/snapshot\"" html))
    (is (re-find #"src=\"/karbosh/assets/js/admin.js\?v=20260606-scroll\"" html))
    (is (re-find #"class=\"admin-table\"" html))
    (is (re-find #"data-label=\"Room\"" html))
    (is (re-find #"\.admin-table\{display:table" html))
    (is (re-find #"\.admin-table thead\{display:table-header-group\}" html))
    (is (re-find #"\.admin-table td::before\{content:none\}" html))
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
