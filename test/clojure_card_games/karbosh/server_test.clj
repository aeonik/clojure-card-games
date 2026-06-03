(ns clojure-card-games.karbosh.server-test
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.admin :as admin]
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

(deftest origin-allowlist-test
  (with-redefs [server/allowed-origins (constantly #{"https://dc3systems.com"})]
    (is (server/origin-allowed?
         {:headers {"origin" "https://dc3systems.com"
                    "host" "dc3systems.com"}}))
    (is (not (server/origin-allowed?
              {:headers {"origin" "https://evil.example"
                         "host" "dc3systems.com"}})))))

(deftest same-host-origin-default-test
  (with-redefs [server/allowed-origins (constantly #{})]
    (is (server/origin-allowed?
         {:headers {"origin" "https://dc3systems.com"
                    "host" "dc3systems.com"}}))
    (is (not (server/origin-allowed?
              {:headers {"origin" "https://evil.example"
                         "host" "dc3systems.com"}})))))

(deftest response-security-headers-test
  (let [headers (:headers (server/response 200 "ok"))]
    (is (= "nosniff" (get headers "X-Content-Type-Options")))
    (is (= "no-referrer" (get headers "Referrer-Policy")))
    (is (= "DENY" (get headers "X-Frame-Options")))
    (is (re-find #"frame-ancestors 'none'"
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
