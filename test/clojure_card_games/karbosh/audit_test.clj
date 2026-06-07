(ns clojure-card-games.karbosh.audit-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.room :as room])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn played-room [room]
  (assoc-in room [:game :hand-history] [{:hand-index 0}]))

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

(deftest audit-readers-return-latest-room-records-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-read-test"
                                                (make-array FileAttribute 0)))
        first-room (played-room (room/new-room "FIRST" 1))
        second-room (played-room (room/new-room "SECOND" 2))]
    (audit/append-record! dir (audit/room-record :room-publish first-room 1000))
    (audit/append-record! dir (audit/room-record :room-delete-idle first-room 3000))
    (audit/append-record! dir (audit/room-record :room-publish second-room 2000))
    (let [first-record (audit/latest-room-record dir "FIRST")
          latest-records (audit/latest-room-records dir)]
      (is (= :room-delete-idle (:type first-record)))
      (is (= 3000 (:logged-at first-record)))
      (is (= ["FIRST" "SECOND"] (mapv :room-id latest-records)))
      (is (= [3000 2000] (mapv :logged-at latest-records))))))

(deftest all-room-records-returns-complete-archive-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-all-test"
                                                (make-array FileAttribute 0)))
        first-room (room/new-room "FIRST" 1)
        second-room (room/new-room "SECOND" 2)]
    (audit/append-record! dir (audit/room-record :room-publish first-room 1000))
    (audit/append-record! dir (audit/room-record :room-delete-idle first-room 3000))
    (audit/append-record! dir (audit/room-record :room-publish second-room 2000))
    (let [records (audit/all-room-records dir)]
      (is (= ["FIRST" "SECOND" "FIRST"] (mapv :room-id records)))
      (is (= [3000 2000 1000] (mapv :logged-at records))))))

(deftest game-history-records-keep-completed-games-and-latest-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-games-test"
                                                (make-array FileAttribute 0)))
        old-game (-> (room/new-room "ROOM1" 17)
                     (played-room)
                     (assoc-in [:game :phase] :game-over))
        current-game (played-room (room/new-room "ROOM1" 99))]
    (audit/append-record! dir (audit/room-record :room-publish old-game 1000))
    (audit/append-record! dir (audit/room-record :room-publish current-game 2000))
    (let [records (audit/game-history-records dir)]
      (is (= [99 17] (mapv audit/game-seed records)))
      (is (= :game-over (get-in (second records) [:room :game :phase])))
      (is (= 17 (-> (audit/room-game-record dir "ROOM1" 17)
                    audit/game-seed))))))

(deftest game-history-records-keep-replayed-seeds-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-replay-test"
                                                (make-array FileAttribute 0)))
        first-game (-> (room/new-room "ROOM1" 17)
                       (played-room)
                       (assoc :game-started-at 111)
                       (assoc-in [:game :phase] :game-over))
        replayed-game (-> (room/new-room "ROOM1" 17)
                          (played-room)
                          (assoc :game-started-at 222)
                          (assoc-in [:game :phase] :game-over))]
    (audit/append-record! dir (audit/room-record :room-publish first-game 1000))
    (audit/append-record! dir (audit/room-record :room-publish replayed-game 2000))
    (let [records (audit/game-history-records dir)]
      (is (= [222 111] (mapv audit/game-timestamp records)))
      (is (= 222 (-> (audit/room-game-record dir "ROOM1" 17 222)
                     audit/game-timestamp)))
      (is (= 111 (-> (audit/room-game-record dir "ROOM1" 17 111)
                     audit/game-timestamp))))))

(deftest live-game-timestamp-falls-back-to-room-created-at-test
  (let [room (-> (room/new-room "LIVE1" 17)
                 (dissoc :game-started-at)
                 (assoc :created-at 1234))
        live-record (audit/room-record :room-live room 2000)
        archived-record (audit/room-record :room-publish room 2000)]
    (is (= 1234 (audit/game-timestamp live-record)))
    (is (= 1234 (audit/game-timestamp archived-record)))))

(deftest game-history-records-dedupe-legacy-publishes-by-created-at-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-dedupe-test"
                                                (make-array FileAttribute 0)))
        game (-> (room/new-room "ROOM1" 17)
                 (played-room)
                 (dissoc :game-started-at)
                 (assoc :created-at 111)
                 (assoc-in [:game :phase] :game-over))]
    (audit/append-record! dir (audit/room-record :room-publish game 1000))
    (audit/append-record! dir (audit/room-record :room-publish game 2000))
    (let [records (audit/game-history-records dir)]
      (is (= 1 (count records)))
      (is (= 111 (audit/game-timestamp (first records))))
      (is (= 2000 (:logged-at (first records)))))))

(deftest zero-hand-records-are-not-game-history-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-zero-test"
                                                (make-array FileAttribute 0)))
        abandoned (room/new-room "EMPTY1" 17)]
    (audit/append-record! dir (audit/room-record :room-publish abandoned 1000))
    (audit/append-record! dir (audit/room-record :room-delete-idle abandoned 2000))
    (is (empty? (audit/game-history-records dir)))
    (is (nil? (audit/latest-room-record dir "EMPTY1")))))

(deftest prune-room-records-removes-zero-hand-archive-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-prune-test"
                                                (make-array FileAttribute 0)))
        abandoned (room/new-room "EMPTY1" 17)
        file (io/file dir "EMPTY1.edn")]
    (audit/append-record! dir (audit/room-record :room-publish abandoned 1000))
    (audit/append-record! dir (audit/room-record :room-delete-idle abandoned 2000))
    (is (.exists file))
    (is (= {:room-id "EMPTY1" :before 2 :after 0 :removed 2}
           (audit/prune-room-records! dir "EMPTY1")))
    (is (not (.exists file)))))

(deftest latest-record-reads-only-final-record-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-audit-latest-test"
                                                (make-array FileAttribute 0)))
        room (room/new-room "LATEST" 7)]
    (audit/append-record! dir (audit/room-record :room-publish room 1000))
    (audit/append-record! dir (audit/room-record :room-close room 2000))
    (let [file (io/file dir "LATEST.edn")
          latest (audit/latest-record file)]
      (is (= :room-close (:type latest)))
      (is (= 2000 (:logged-at latest)))
      (is (= "LATEST" (:room-id latest))))))
