(ns clojure-card-games.karbosh.runtime-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.runtime :as runtime]))

(deftest current-handler-test
  (let [old-handler @runtime/handler*]
    (try
      (reset! runtime/handler* nil)
      (is (= 503 (:status (runtime/current-handler {}))))
      (is (= :ok (runtime/install-handler! (fn [_] {:status 200 :body "ok"}))))
      (is (= {:status 200 :body "ok"} (runtime/current-handler {})))
      (finally
        (reset! runtime/handler* old-handler)))))

(deftest websocket-dispatch-test
  (let [old-handlers @runtime/ws-handlers*
        messages (atom [])
        closes (atom [])]
    (try
      (runtime/install-ws-handlers!
       {:on-message (fn [conn-id out session raw]
                      (swap! messages conj [conn-id out @session raw]))
        :on-close (fn [conn-id in out session]
                    (swap! closes conj [conn-id in out @session]))})
      (runtime/dispatch-message :conn :out (atom {:room-id "ABC123"}) "ping")
      (runtime/dispatch-close :conn :in :out (atom {:room-id "ABC123"}))
      (is (= [[:conn :out {:room-id "ABC123"} "ping"]] @messages))
      (is (= [[:conn :in :out {:room-id "ABC123"}]] @closes))
      (finally
        (reset! runtime/ws-handlers* old-handlers)))))
