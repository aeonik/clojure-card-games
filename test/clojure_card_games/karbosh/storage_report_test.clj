(ns clojure-card-games.karbosh.storage-report-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.storage :as storage]
            [clojure-card-games.karbosh.storage-report :as storage-report])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix
                                      (make-array FileAttribute 0))))

(defn played-room [room-id seed]
  (assoc-in (room/new-room room-id seed)
            [:game :hand-history]
            [{:hand-index 0}]))

(deftest storage-report-summarizes-room-store-test
  (let [room-dir (temp-dir "karbosh-storage-report-room-test")
        audit-dir (temp-dir "karbosh-storage-report-audit-test")]
    (storage/write-room! room-dir (played-room "PLAYED" 17))
    (storage/write-room! room-dir (room/new-room "EMPTY1" 99))
    (spit (io/file room-dir "BROKEN.edn") "{")
    (spit (io/file audit-dir "legacy.edn") "legacy")
    (let [report (storage-report/report {:room-dir room-dir
                                         :audit-dir audit-dir})]
      (is (= 3 (:room-files report)))
      (is (pos? (:room-bytes report)))
      (is (pos? (:audit-bytes report)))
      (is (= 1 (:malformed-room-files report)))
      (is (= 1 (:zero-hand-room-files report)))
      (is (= ["BROKEN"] (mapv :room-id (:malformed report))))
      (is (= ["EMPTY1"] (mapv :room-id (:zero-hand report))))
      (is (= #{"BROKEN" "EMPTY1" "PLAYED"}
             (set (map :room-id (:largest-rooms report))))))))
