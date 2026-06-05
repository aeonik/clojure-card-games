(ns clojure-card-games.karbosh.audit-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.room :as room])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest sanitize-room-removes-live-connections-test
  (let [state (-> (room/new-room "ABC123" 17)
                  (assoc :connections {:conn {:player :player1
                                               :out :channel}}))
        snapshot (audit/sanitize-room state)]
    (is (= "ABC123" (:id snapshot)))
    (is (= 17 (:seed snapshot)))
    (is (not (contains? snapshot :connections)))
    (is (get-in snapshot [:game :initial-seed]))))

(deftest audit-writer-appends-edn-records-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-test"
                                                (make-array FileAttribute 0)))
        state (room/new-room "ABC123" 17)]
    (try
      (audit/start! {:enabled? true :dir dir})
      (is (true? (audit/record-room! :room-publish state)))
      (audit/stop!)
      (let [file (io/file dir "ABC123.edn")
            records (with-open [r (io/reader file)]
                      (doall (map edn/read-string (line-seq r))))
            record (first records)]
        (is (= 1 (count records)))
        (is (= audit/schema-version (:schema record)))
        (is (= :room-publish (:type record)))
        (is (= "ABC123" (:room-id record)))
        (is (= 17 (get-in record [:room :seed])))
        (is (not (contains? (:room record) :connections))))
      (finally
        (audit/stop!)))))
