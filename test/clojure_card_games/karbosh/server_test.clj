(ns clojure-card-games.karbosh.server-test
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure-card-games.karbosh.admin :as admin]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.server :as server]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.storage :as storage]
            [clojure-card-games.karbosh.workbench :as workbench])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def completed-trick
  [{:player :player1 :card [:K :♥]}
   {:player :player2 :card [:A :♥]}
   {:player :player3 :card [:Q :♥]}
   {:player :player4 :card [:J :♥]}
   {:player :player5 :card [10 :♥]}
   {:player :player6 :card [9 :♥]}])

(def completed-initial-hands
  {:player1 [[:K :♥] [9 :♣] [9 :♣] [9 :♦]
             [9 :♦] [9 :♠] [9 :♠] [10 :♣]]
   :player2 [[:A :♥] [10 :♣] [10 :♦] [10 :♦]
             [10 :♠] [10 :♠] [:J :♣] [:J :♣]]
   :player3 [[:Q :♥] [:J :♦] [:J :♦] [:J :♠]
             [:J :♠] [:Q :♣] [:Q :♣] [:Q :♦]]
   :player4 [[:J :♥] [:Q :♦] [:Q :♠] [:Q :♠]
             [:K :♣] [:K :♣] [:K :♦] [:K :♦]]
   :player5 [[10 :♥] [:K :♠] [:K :♠] [:A :♣]
             [:A :♣] [:A :♦] [:A :♦] [:A :♠]]
   :player6 [[9 :♥] [9 :♥] [10 :♥] [:J :♥]
             [:Q :♥] [:K :♥] [:A :♥] [:A :♠]]})

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
                  :initial-hands completed-initial-hands
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
        (workbench/clear!)
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
    (is (= server/ultra-fast-trick-complete-delay-ms
           (server/bot-turn-delay-ms (assoc room :speed-mode :ultra-fast))))
    (is (= server/bot-action-delay-ms
           (server/bot-turn-delay-ms (assoc-in room [:game :current-trick]
                                               [{:player :player1 :card [:K :♥]}]))))
    (is (= server/fast-bot-action-delay-ms
           (server/bot-turn-delay-ms (-> room
                                         (assoc :fast-mode? true)
                                         (assoc-in [:game :current-trick]
                                                   [{:player :player1 :card [:K :♥]}])))))
    (is (= server/ultra-fast-bot-action-delay-ms
           (server/bot-turn-delay-ms (-> room
                                         (assoc :speed-mode :ultra-fast)
                                         (assoc-in [:game :current-trick]
                                                   [{:player :player1 :card [:K :♥]}])))))))

(deftest reloadable-namespaces-order-test
  (let [namespaces (vec server/reloadable-namespaces)
        analysis-index (.indexOf namespaces 'clojure-card-games.karbosh.analysis)
        parallel-index (.indexOf namespaces 'clojure-card-games.karbosh.parallel)
        bot-index (.indexOf namespaces 'clojure-card-games.karbosh.bot)
        room-index (.indexOf namespaces 'clojure-card-games.karbosh.room)
        trick-lab-index (.indexOf namespaces 'clojure-card-games.karbosh.trick-lab)
        admin-index (.indexOf namespaces 'clojure-card-games.karbosh.admin)]
    (is (not= -1 parallel-index))
    (is (not= -1 analysis-index))
    (is (not= -1 bot-index))
    (is (not= -1 room-index))
    (is (not= -1 trick-lab-index))
    (is (< parallel-index analysis-index))
    (is (< analysis-index bot-index))
    (is (< bot-index room-index))
    (is (< room-index trick-lab-index))
    (is (< trick-lab-index admin-index))))

(deftest admin-basic-auth-test
  (with-redefs [server/admin-user (constantly "admin")
                server/admin-password (constantly "secret")]
    (is (server/admin-authorized?
         {:headers {"authorization" "Basic YWRtaW46c2VjcmV0"}}))
    (is (not (server/admin-authorized?
              {:headers {"authorization" "Basic YWRtaW46d3Jvbmc="}})))
    (is (not (server/admin-authorized?
              {:headers {"authorization" "Basic not-base64"}})))))

(deftest admin-cookie-auth-test
  (with-redefs [server/admin-user (constantly "admin")
                server/admin-password (constantly "secret")]
    (let [cookie (str server/admin-session-cookie-name
                      "="
                      (server/admin-session-cookie-value))]
      (is (server/admin-authorized?
           {:headers {"cookie" cookie}}))
      (is (not (server/admin-authorized?
                {:headers {"cookie" (str server/admin-session-cookie-name
                                         "=wrong")}}))))))

(deftest admin-login-sets-session-cookie-test
  (with-redefs [server/admin-user (constantly "admin")
                server/admin-password (constantly "secret")]
    (let [response (server/handler
                    {:request-method :post
                     :uri "/karbosh/admin/login"
                     :headers {"host" "dc3systems.com"}
                     :body "username=admin&karbosh_admin_password=secret&return=/karbosh/admin/workbench/ABC123"})
          cookie (get-in response [:headers "Set-Cookie"])]
      (is (= 303 (:status response)))
      (is (= "/karbosh/admin/workbench/ABC123"
             (get-in response [:headers "Location"])))
      (is (re-find (re-pattern server/admin-session-cookie-name) cookie))
      (is (re-find #"HttpOnly" cookie))
      (is (re-find #"SameSite=Lax" cookie)))))

(deftest admin-html-routes-redirect-to-cookie-login-test
  (with-redefs [server/admin-user (constantly "admin")
                server/admin-password (constantly "secret")]
    (let [response (server/handler
                    {:request-method :get
                     :uri "/karbosh/admin"
                     :headers {"host" "dc3systems.com"}})]
      (is (= 303 (:status response)))
      (is (re-find #"/karbosh/admin/login"
                   (get-in response [:headers "Location"])))
      (is (not (contains? (:headers response) "WWW-Authenticate"))))))

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

(deftest admin-workbench-renders-frozen-room-test
  (let [old-rooms @server/rooms
        room (-> (room/fill-bots (room/new-room "ABC123" 9))
                 (assoc-in [:game :phase] :trick-playing)
                 (assoc-in [:game :trump] :♠)
                 (assoc-in [:game :current-trick]
                           [{:player :player1 :card [:A :♠]}
                            {:player :player2 :card [10 :♠]}])
                 (assoc-in [:game :completed-tricks]
                           [[{:player :player4 :card [:J :♠]}
                             {:player :player5 :card [:Q :♠]}
                             {:player :player6 :card [9 :♠]}]]))]
    (try
      (reset! server/rooms {"ABC123" room})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :get
                         :uri "/karbosh/admin/workbench/ABC123"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})]
          (is (= 200 (:status response)))
          (is (re-find #"Workbench ABC123" (:body response)))
          (is (re-find #"Board state" (:body response)))
          (is (re-find #"Current trick" (:body response)))
          (is (re-find #"A♠" (:body response)))
          (is (re-find #"Played tricks" (:body response)))
          (is (re-find #"Trick 1" (:body response)))
          (is (re-find #"is-winning" (:body response)))
          (is (re-find #"wb-board-card-risk" (:body response)))
          (is (re-find #"name=\"observer\"[^>]*value=\"player1\"" (:body response)))
          (is (re-find #"href=\"/karbosh/assets/css/admin\.css\" rel=\"stylesheet\""
                       (:body response)))
          (is (re-find #"href=\"/karbosh/assets/css/workbench\.css\" rel=\"stylesheet\""
                       (:body response)))
          (is (re-find #"src=\"/karbosh/assets/js/workbench\.js\?v=20260610-workbench-forms\""
                       (:body response)))
          (is (not (re-find #"this\.form\.submit\(\)"
                            (:body response))))
          (is (not (re-find #">Apply</button>"
                            (:body response))))
          (is (re-find #"AI strategy controls" (:body response)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-workbench-index-renders-room-picker-test
  (let [old-rooms @server/rooms
        room (room/fill-bots (room/new-room "ABC123" 9))]
    (try
      (reset! server/rooms {"ABC123" room})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :get
                         :uri "/karbosh/admin/workbench/"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})]
          (is (= 200 (:status response)))
          (is (re-find #"AI workbench" (:body response)))
          (is (re-find #"href=\"/karbosh/admin/workbench/ABC123\""
                       (:body response)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-workbench-index-renders-durable-zero-hand-room-test
  (let [old-rooms @server/rooms
        room (room/new-room "EMPTY1" 9)]
    (try
      (storage/write-room! (server/room-dir) room)
      (reset! server/rooms {})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :get
                         :uri "/karbosh/admin/workbench/"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"}})]
          (is (= 200 (:status response)))
          (is (re-find #"href=\"/karbosh/admin/workbench/EMPTY1\""
                       (:body response)))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-workbench-index-room-query-redirects-test
  (with-redefs [server/admin-user (constantly "admin")
                server/admin-password (constantly "secret")]
    (let [response (server/handler
                    {:request-method :get
                     :uri "/karbosh/admin/workbench"
                     :query-string "room=abc123"
                     :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                               "host" "dc3systems.com"}})]
      (is (= 303 (:status response)))
      (is (= "/karbosh/admin/workbench/ABC123"
             (get-in response [:headers "Location"]))))))

(deftest admin-workbench-manual-actions-do-not-touch-live-room-test
  (let [old-rooms @server/rooms
        room (room/fill-bots (room/new-room "ABC123" 9))]
    (try
      (reset! server/rooms {"ABC123" room})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :post
                         :uri "/karbosh/admin/workbench/ABC123"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"
                                   "origin" "https://debug-browser.example"}
                         :body "action=manual&bid-type=pass"})]
          (is (= 303 (:status response)))
          (is (= "/karbosh/admin/workbench/ABC123"
                 (get-in response [:headers "Location"])))
          (is (= :player2
                 (get-in @workbench/sessions* ["ABC123" :room :game :current-player])))
          (is (= :player1
                 (get-in @server/rooms ["ABC123" :game :current-player])))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-workbench-seat-click-switches-to-player-view-test
  (let [old-rooms @server/rooms
        room (-> (room/fill-bots (room/new-room "ABC123" 9))
                 (assoc-in [:game :phase] :trick-playing)
                 (assoc-in [:game :trump] :♣)
                 (assoc-in [:game :completed-tricks]
                           [[{:player :player1 :card [:A :♣]}
                             {:player :player2 :card [:A :♣]}
                             {:player :player3 :card [9 :♠]}
                             {:player :player4 :card [10 :♥]}]]))]
    (try
      (reset! server/rooms {"ABC123" room})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :post
                         :uri "/karbosh/admin/workbench/ABC123"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"
                                   "origin" "https://debug-browser.example"}
                         :body "action=view&view-mode=ai&observer=player4"})]
          (is (= 303 (:status response)))
          (is (= :ai (get-in @workbench/sessions* ["ABC123" :view-mode])))
          (is (= :player4 (get-in @workbench/sessions* ["ABC123" :observer]))))
        (let [view-response (server/handler
                             {:request-method :get
                              :uri "/karbosh/admin/workbench/ABC123"
                              :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                        "host" "dc3systems.com"}})]
          (is (= 200 (:status view-response)))
          (is (re-find #"Return to God&apos;s eye view" (:body view-response)))
          (is (re-find #"name=\"view-mode\"[^>]*value=\"god\"" (:body view-response)))
          (is (re-find #"Hidden exhausted" (:body view-response)))
          (is (re-find #"A♣" (:body view-response)))
          (is (re-find #"wb-board-voids" (:body view-response)))
          (is (re-find #"Void odds" (:body view-response)))
          (is (re-find #"suit club\">♣</span></span><b>100\.0%"
                       (:body view-response))))
        (let [toggle-response (server/handler
                               {:request-method :post
                                :uri "/karbosh/admin/workbench/ABC123"
                                :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                          "host" "dc3systems.com"
                                          "origin" "https://debug-browser.example"}
                                :body "action=view&view-mode=god&observer=player4"})]
          (is (= 303 (:status toggle-response)))
          (is (= :god (get-in @workbench/sessions* ["ABC123" :view-mode])))))
      (finally
        (reset! server/rooms old-rooms)))))

(deftest admin-workbench-bookmark-records-reproducible-frozen-coordinate-test
  (let [old-rooms @server/rooms
        room (-> (room/fill-bots (room/new-room "ABC123" 111))
                 (assoc :game-index 3)
                 (assoc-in [:game :initial-seed] 222)
                 (assoc-in [:game :hand-index] 4)
                 (assoc-in [:game :phase] :trick-playing)
                 (assoc-in [:game :trump] :♠)
                 (assoc-in [:game :current-player] :player3)
                 (assoc-in [:game :completed-tricks]
                           [[{:player :player1 :card [:A :♠]}
                             {:player :player2 :card [10 :♠]}]])
                 (assoc-in [:game :current-trick]
                           [{:player :player3 :card [:Q :♠]}
                            {:player :player4 :card [:K :♠]}])
                 (assoc-in [:game :hand-deals]
                           [{:hand-index 0 :seed 111 :hands {}}
                            {:hand-index 4 :seed 12345 :hands {}}
                            {:hand-index 4 :seed 98765 :hands {}}]))]
    (try
      (reset! server/rooms {"ABC123" room})
      (with-redefs [server/admin-user (constantly "admin")
                    server/admin-password (constantly "secret")]
        (let [response (server/handler
                        {:request-method :post
                         :uri "/karbosh/admin/workbench/ABC123"
                         :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                   "host" "dc3systems.com"
                                   "origin" "https://debug-browser.example"}
                         :body "action=bookmark&note=interesting+state"})
              bookmark (first (get @workbench/bookmarks* "ABC123"))
              coordinate (:coordinate bookmark)]
          (is (= 303 (:status response)))
          (is (= {:room-id "ABC123"
                  :room-seed 111
                  :game-index 3
                  :game-seed 222
                  :game-started-at (:game-started-at room)
                  :hand-index 4
                  :hand-number 5
                  :hand-seed 98765
                  :hand-deal-seeds [12345 98765]
                  :phase :trick-playing
                  :current-player :player3
                  :trick-index 1
                  :trick-number 2
                  :completed-tricks 1
                  :current-trick-cards 2}
                 coordinate))
          (is (= :player3
                 (get-in (:room bookmark) [:game :current-player])))
          (is (= (:id bookmark)
                 (->> (audit/room-records (server/audit-dir) "ABC123")
                      (keep workbench/bookmark-from-record)
                      first
                      :id)))
          (swap! workbench/sessions*
                 assoc-in
                 ["ABC123" :room :game :current-player]
                 :player6)
          (let [restore-response (server/handler
                                  {:request-method :post
                                   :uri "/karbosh/admin/workbench/ABC123"
                                   :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                             "host" "dc3systems.com"
                                             "origin" "https://debug-browser.example"}
                                   :body (str "action=restore-bookmark&bookmark-id="
                                              (:id bookmark))})]
            (is (= 303 (:status restore-response)))
            (is (= :player3
                   (get-in @workbench/sessions* ["ABC123" :room :game :current-player])))
            (is (= 4
                   (get-in @workbench/sessions* ["ABC123" :room :game :hand-index])))
            (is (= :player3
                   (get-in @server/rooms ["ABC123" :game :current-player]))))
          (let [view-response (server/handler
                               {:request-method :get
                                :uri "/karbosh/admin/workbench/ABC123"
                                :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                          "host" "dc3systems.com"}})]
            (is (= 200 (:status view-response)))
            (is (re-find #"Game 4 / Hand 5 / Trick playing" (:body view-response)))
            (is (re-find #"Hand seed" (:body view-response)))
            (is (re-find #"98765" (:body view-response)))
            (is (re-find #"12345 -&gt; 98765" (:body view-response)))
            (is (.contains (:body view-response)
                           (str "/karbosh/admin/history/ABC123/222/"
                                (:game-started-at room)
                                "/snapshot/hands/4")))
            (is (re-find #"Restore frozen point" (:body view-response))))
          (workbench/clear!)
          (let [loaded-response (server/handler
                                 {:request-method :get
                                  :uri "/karbosh/admin/workbench/ABC123"
                                  :headers {"authorization" "Basic YWRtaW46c2VjcmV0"
                                            "host" "dc3systems.com"}})]
            (is (= 200 (:status loaded-response)))
            (is (re-find #"interesting state" (:body loaded-response)))
            (is (re-find #"Restore frozen point" (:body loaded-response))))))
      (finally
        (reset! server/rooms old-rooms)))))

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
    (is (server/origin-allowed?
         {:headers {"origin" "http://localhost:8091"
                    "host" "127.0.0.1:8091"}}))
    (is (server/origin-allowed?
         {:headers {"origin" "http://127.0.0.1:8091"
                    "host" "localhost:8091"}}))
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
                                                        :speed-mode :ultra-fast
                                                        :seed -42})))
        (is (true? (get-in @server/rooms ["PUB123" :public?])))
        (is (true? (get-in @server/rooms ["PUB123" :fast-mode?])))
        (is (= :ultra-fast (get-in @server/rooms ["PUB123" :speed-mode])))
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

(deftest create-room-without-explicit-seed-uses-random-seed-test
  (let [out (async/chan 1)
        old-rooms @server/rooms]
    (try
      (reset! server/rooms {})
      (with-redefs [server/unique-room-id (constantly "RND123")
                    room/random-seed (constantly -1194148630)]
        (is (= "RND123" (server/create-room! :conn out {:name "Human"})))
        (is (= -1194148630 (get-in @server/rooms ["RND123" :seed])))
        (is (= -1194148630
               (get-in @server/rooms ["RND123" :game :initial-seed])))
        (is (not= (get-in @server/rooms ["RND123" :seed])
                  (get-in @server/rooms ["RND123" :created-at]))))
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
          (is (re-find #"href=\"/karbosh/assets/css/admin\.css\" rel=\"stylesheet\""
                       (:body response)))
          (is (re-find #"href=\"/karbosh/assets/css/snapshot\.css\" rel=\"stylesheet\""
                       (:body response)))
          (is (re-find #"Raw EDN" (:body response)))
          (is (= 200 (:status detail-response)))
          (is (re-find #"Karbosh hand detail" (:body detail-response)))
          (is (re-find #"Play by Play" (:body detail-response)))
          (is (re-find #"AI Policies" (:body detail-response)))
          (is (re-find #"ai-policy-card" (:body detail-response)))
          (is (re-find #"Policies" (:body detail-response)))
          (is (re-find #"Engines" (:body detail-response)))
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
          (is (re-find #"href=\"/karbosh/assets/css/snapshot\.css\" rel=\"stylesheet\""
                       (:body detail-response)))
          (is (re-find #"starting-hand-row" (:body detail-response)))
          (is (re-find #"Analyze" (:body detail-response)))
          (is (re-find #"href=\"/karbosh/admin/rooms/ABC123/snapshot/hands/0/tricks/0/analysis\""
                       (:body detail-response)))
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

(deftest public-rest-snapshot-game-hand-trick-api-test
  (let [old-rooms @server/rooms
        room (assoc (completed-room "ABC123" 9)
                    :game-started-at 111
                    :updated-at 222)]
    (try
      (reset! server/rooms {"ABC123" room})
      (let [snapshot-response (server/handler
                               {:request-method :get
                                :uri "/karbosh/api/rooms/ABC123/snapshot"
                                :headers {"host" "dc3systems.com"}})
            games-response (server/handler
                            {:request-method :get
                             :uri "/karbosh/api/rooms/ABC123/games"
                             :headers {"host" "dc3systems.com"}})
            game-response (server/handler
                           {:request-method :get
                            :uri "/karbosh/api/rooms/ABC123/games/9/111"
                            :headers {"host" "dc3systems.com"}})
            hand-response (server/handler
                           {:request-method :get
                            :uri "/karbosh/api/rooms/ABC123/games/9/111/hands/0"
                            :headers {"host" "dc3systems.com"}})
            trick-response (server/handler
                            {:request-method :get
                             :uri "/karbosh/api/rooms/ABC123/games/9/111/hands/0/tricks/0"
                             :headers {"host" "dc3systems.com"}})
            snapshot (edn/read-string (:body snapshot-response))
            games (edn/read-string (:body games-response))
            game (edn/read-string (:body game-response))
            hand (edn/read-string (:body hand-response))
            trick (edn/read-string (:body trick-response))]
        (is (= 200 (:status snapshot-response)))
        (is (= :room-snapshot (:kind snapshot)))
        (is (= "ABC123" (:room-id snapshot)))
        (is (= "/karbosh/api/rooms/ABC123/games"
               (get-in snapshot [:links :games])))
        (is (= 200 (:status games-response)))
        (is (= :room-games (:kind games)))
        (is (= 1 (count (:games games))))
        (is (= "/karbosh/api/rooms/ABC123/games/9/111"
               (get-in games [:games 0 :links :game])))
        (is (= 200 (:status game-response)))
        (is (= :game (:kind game)))
        (is (= 9 (:seed game)))
        (is (= 111 (:timestamp game)))
        (is (= :game-over (get-in game [:game :phase])))
        (is (= 200 (:status hand-response)))
        (is (= :hand (:kind hand)))
        (is (= 0 (:hand-index hand)))
        (is (= :♠ (get-in hand [:hand :trump])))
        (is (= "/karbosh/api/rooms/ABC123/games/9/111/hands/0/tricks/0"
               (get-in hand [:links :tricks 0 :href])))
        (is (= 200 (:status trick-response)))
        (is (= :trick (:kind trick)))
        (is (= :completed (:trick-status trick)))
        (is (= :player2 (:winning-player trick)))
        (is (= 2 (:winning-team trick)))
        (is (= completed-trick (:trick trick))))
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
              game-analysis-response (server/handler
                                      {:request-method :get
                                       :uri "/karbosh/admin/history/OLD123/17/111/snapshot/hands/0/tricks/0/analysis"
                                       :query-string "samples=2"
                                   :headers {"host" "dc3systems.com"}})
              room-edn-body (edn/read-string (:body room-edn-response))
              game-edn-body (edn/read-string (:body game-edn-response))]
          (is (= 303 (:status dashboard-response)))
          (is (re-find #"/karbosh/admin/login"
                       (get-in dashboard-response [:headers "Location"])))
          (is (= 303 (:status history-response)))
          (is (re-find #"/karbosh/admin/login"
                       (get-in history-response [:headers "Location"])))
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
          (is (= 200 (:status game-analysis-response)))
          (is (re-find #"Counterfactual trick analysis"
                       (:body game-analysis-response)))
          (is (re-find #"Monte Carlo by legal lead"
                       (:body game-analysis-response)))
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
      (server/set-fast-mode! "ABC123" out :ultra-fast)
      (let [message (async/<!! out)]
        (is (true? (get-in @server/rooms ["ABC123" :fast-mode?])))
        (is (= :ultra-fast (get-in @server/rooms ["ABC123" :speed-mode])))
        (is (= :state (:op message)))
        (is (true? (get-in message [:view :fast-mode?])))
        (is (= :ultra-fast (get-in message [:view :speed-mode]))))
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

(deftest idle-sweep-unloads-zero-hand-room-without-deleting-it-test
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
        (is (.exists file))
        (is (storage/room-exists? (server/room-dir) "EMPTY1"))
        (is (= "EMPTY1" (:id (storage/read-room (server/room-dir) "EMPTY1")))))
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

(deftest idle-sweep-keeps-zero-hand-room-durable-test
  (let [old-rooms @server/rooms
        room (-> (room/new-room "EMPTY1" 9)
                 (assoc :empty-since 0
                        :connections {}))]
    (try
      (reset! server/rooms {"EMPTY1" room})
      (with-redefs [server/idle-room-ms (constantly 1000)]
        (is (= ["EMPTY1"] (server/close-idle-rooms!)))
        (is (not (contains? @server/rooms "EMPTY1")))
        (is (storage/room-exists? (server/room-dir) "EMPTY1"))
        (is (= "EMPTY1" (:id (storage/read-room (server/room-dir) "EMPTY1")))))
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

(deftest seat-bot-seats-specific-bot-and-broadcasts-test
  (let [out (async/chan 2)
        old-rooms @server/rooms
        room (-> (room/new-room "ABC123" 9)
                 (room/join-room {:conn-id :owner
                                  :out out
                                  :player :player1
                                  :name "Owner"})
                 (room/ensure-owner))]
    (try
      (reset! server/rooms {"ABC123" room})
      (server/seat-bot! "ABC123" out :player2 "Deal-E")
      (let [message (async/<!! out)
            room (get @server/rooms "ABC123")]
        (is (= :state (:op message)))
        (is (= "Deal-E" (get-in room [:seats :player2 :name])))
        (is (true? (get-in room [:seats :player2 :bot?])))
        (is (not-any? #(= "Deal-E" (:name %))
                      (get-in message [:view :available-bot-personas]))))
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

(deftest admin-history-panel-keeps-latest-played-rooms-test
  (let [records (mapv (fn [i]
                        (let [room-id (format "ROOM%02d" i)]
                          {:room-id room-id
                           :logged-at i
                           :type :room-durable
                           :room (played-room (room/new-room room-id i))}))
                      (range 12))
        active-room (connected-room (played-room (room/new-room "ROOM11" 11)))
        latest (admin/historical-room-records {"ROOM11" active-room} records)]
    (is (= 10 (count latest)))
    (is (= "ROOM11" (:room-id (first latest))))
    (is (some #(= "ROOM11" (:room-id %)) latest))
    (is (not-any? #(= "ROOM00" (:room-id %)) latest))
    (is (not-any? #(= "ROOM01" (:room-id %)) latest))))

(deftest admin-history-stats-count-games-inside-rooms-test
  (let [previous (completed-room "MULTI1" 101)
        current (completed-room "MULTI1" 202)
        room (assoc current
                    :games [{:game (:game previous)
                             :ended-reason :completed
                             :completed-at 111}])
        records [{:room-id "MULTI1"
                  :logged-at 222
                  :type :room-durable
                  :room room}]
        stats (into {} (map (juxt :label :value) (admin/historical-stats records)))
        trends (into {} (map (juxt :bid identity) (admin/bid-trends records)))]
    (is (= 1 (get stats "Rooms shown")))
    (is (= 2 (get stats "Games")))
    (is (= 2 (get stats "Completed games")))
    (is (= 2 (get stats "Total hands")))
    (is (= 2 (get-in trends ["Bid 4" :attempts])))
    (is (= 2 (get-in trends ["Bid 4" :made])))))

(deftest room-snapshot-renders-stored-room-games-test
  (let [previous (-> (completed-room "MULTI1" 101)
                     (assoc :game-started-at 111)
                     (assoc-in [:game :hand-history 0 :hand-index] 0))
        current (-> (completed-room "MULTI1" 202)
                    (assoc :game-started-at 222)
                    (assoc :game-index 1)
                    (assoc-in [:game :hand-history 0 :hand-index] 1))
        room (assoc current
                    :games [{:game-index 0
                             :seed 101
                             :started-at 111
                             :completed-at 120
                             :game (:game previous)}])
        html (admin/render-room-snapshot room)]
    (is (re-find #"Games in this room" html))
    (is (re-find #"Game 1" html))
    (is (re-find #"Game 2 / current" html))
    (is (re-find #"/karbosh/admin/history/MULTI1/101/111/snapshot" html))
    (is (re-find #"/karbosh/admin/history/MULTI1/202/222/snapshot" html))))

(deftest durable-game-records-preserve-stored-game-hands-test
  (let [previous (-> (completed-room "MULTI1" 101)
                     (assoc :game-started-at 111)
                     (assoc-in [:game :hand-history]
                               [{:hand-index 0
                                 :marker :previous}]))
        current (-> (completed-room "MULTI1" 202)
                    (assoc :game-started-at 222)
                    (assoc :game-index 1)
                    (assoc-in [:game :hand-history]
                              [{:hand-index 0
                                :marker :current}]))
        room (assoc current
                    :games [{:game-index 0
                             :seed 101
                             :started-at 111
                             :completed-at 120
                             :game (:game previous)}])
        by-seed (into {} (map (fn [record]
                                [(get-in record [:room :game :initial-seed])
                                 (get-in record [:room :game :hand-history])])
                              (server/durable-game-records-for-room room)))]
    (is (= [{:hand-index 0 :marker :previous}] (get by-seed 101)))
    (is (= [{:hand-index 0 :marker :current}] (get by-seed 202)))))

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
    (is (re-find #"src=\"/karbosh/assets/js/admin.js\?v=20260608-history-preserve\"" html))
    (is (re-find #"class=\"admin-table\"" html))
    (is (re-find #"data-label=\"Room\"" html))
    (is (re-find #"href=\"/karbosh/assets/css/admin\.css\" rel=\"stylesheet\"" html))
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
