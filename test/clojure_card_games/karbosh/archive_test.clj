(ns clojure-card-games.karbosh.archive-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.archive :as archive]
            [clojure-card-games.karbosh.audit :as audit]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.storage :as storage])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix
                                      (make-array FileAttribute 0))))

(defn played-room
  ([room-id seed started-at]
   (played-room room-id seed started-at :bidding))
  ([room-id seed started-at phase]
   (-> (room/new-room room-id seed)
       (assoc :game-started-at started-at)
       (assoc-in [:game :phase] phase)
       (assoc-in [:game :hand-history] [{:hand-index 0}]))))

(deftest compact-archive-writes-durable-room-and-backs-up-legacy-audit-test
  (let [audit-dir (temp-dir "karbosh-archive-audit-test")
        room-dir (temp-dir "karbosh-archive-room-test")
        first-game (played-room "ROOM1" 17 111 :game-over)
        current-game (played-room "ROOM1" 99 222)]
    (audit/append-record! audit-dir (audit/room-record :room-publish first-game 1000))
    (audit/append-record! audit-dir (audit/room-record :room-publish current-game 2000))
    (let [dry-run (archive/compact-archive! {:audit-dir audit-dir
                                             :room-dir room-dir})
          compacted (archive/compact-archive! {:audit-dir audit-dir
                                               :room-dir room-dir
                                               :confirm archive/confirm-token})
          loaded (storage/read-room room-dir "ROOM1")]
      (is (= :dry-run (:status dry-run)))
      (is (= :compacted (:status compacted)))
      (is (= 1 (:rooms compacted)))
      (is (= 1 (:history-games compacted)))
      (is (some? (:backup-dir compacted)))
      (is (.exists (io/file (:backup-dir compacted))))
      (is (.exists audit-dir))
      (is (empty? (audit/room-files audit-dir)))
      (is (= 99 (get-in loaded [:game :initial-seed])))
      (is (= 17 (:seed (first (:games loaded)))))
      (is (= 0 (:game-index (first (:games loaded)))))
      (is (= 1 (:game-index loaded))))))

(deftest compact-archive-keeps-latest-completed-game-current-test
  (let [audit-dir (temp-dir "karbosh-archive-complete-audit-test")
        room-dir (temp-dir "karbosh-archive-complete-room-test")
        first-game (played-room "ROOM1" 17 111 :game-over)
        latest-game (played-room "ROOM1" 99 222 :game-over)]
    (audit/append-record! audit-dir (audit/room-record :room-publish first-game 1000))
    (audit/append-record! audit-dir (audit/room-record :room-publish latest-game 2000))
    (archive/compact-archive! {:audit-dir audit-dir
                               :room-dir room-dir
                               :confirm archive/confirm-token})
    (let [loaded (storage/read-room room-dir "ROOM1")]
      (is (= :game-over (get-in loaded [:game :phase])))
      (is (= 99 (get-in loaded [:game :initial-seed])))
      (is (= [17] (mapv :seed (:games loaded)))))))

(deftest compact-archive-merges-with-existing-durable-room-test
  (let [audit-dir (temp-dir "karbosh-archive-merge-audit-test")
        room-dir (temp-dir "karbosh-archive-merge-room-test")
        legacy-game (played-room "ROOM1" 17 111 :game-over)
        current-durable (played-room "ROOM1" 333 333)]
    (audit/append-record! audit-dir (audit/room-record :room-publish legacy-game 1000))
    (storage/write-room! room-dir current-durable)
    (archive/compact-archive! {:audit-dir audit-dir
                               :room-dir room-dir
                               :confirm archive/confirm-token})
    (let [loaded (storage/read-room room-dir "ROOM1")]
      (is (= 333 (get-in loaded [:game :initial-seed])))
      (is (= [17] (mapv :seed (:games loaded))))
      (is (= 1 (:game-index loaded))))))
