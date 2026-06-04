(ns clojure-card-games.karbosh.repl-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.repl :as repl]
            [nrepl.server :as nrepl]))

(deftest safe-repl-bind-test
  (is (repl/safe-repl-bind? "127.0.0.1"))
  (is (repl/safe-repl-bind? "localhost"))
  (is (repl/safe-repl-bind? "::1"))
  (is (not (repl/safe-repl-bind? "0.0.0.0")))
  (is (not (repl/safe-repl-bind? "192.168.1.20"))))

(deftest refuses-unsafe-repl-bind-before-starting-test
  (let [started? (atom false)]
    (with-redefs [nrepl/start-server (fn [& _]
                                       (reset! started? true))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Refusing to start production REPL"
           (repl/start! {:bind "0.0.0.0" :port 7888})))
      (is (false? @started?)))))

(deftest starts-and-stops-loopback-repl-test
  (let [old-server @repl/server*
        stopped? (atom false)]
    (try
      (reset! repl/server* nil)
      (with-redefs [nrepl/start-server (fn [& {:keys [bind port]}]
                                         {:bind bind :port port})
                    nrepl/stop-server (fn [_] (reset! stopped? true))]
        (is (= {:status :started
                :bind "127.0.0.1"
                :port 7888}
               (binding [*out* (java.io.StringWriter.)]
                 (repl/start! {:bind "127.0.0.1" :port 7888}))))
        (is (= {:bind "127.0.0.1" :port 7888} @repl/server*))
        (is (= {:status :stopped} (repl/stop!)))
        (is (true? @stopped?))
        (is (nil? @repl/server*)))
      (finally
        (reset! repl/server* old-server)))))
