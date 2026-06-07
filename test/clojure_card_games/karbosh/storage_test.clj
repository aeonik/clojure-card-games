(ns clojure-card-games.karbosh.storage-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.storage :as storage])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest write-and-read-room-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-storage-test"
                                                (make-array FileAttribute 0)))
        original (-> (room/new-room "ABC123" 9)
                     (room/seat-player :player1 "Dave")
                     (room/seat-bot :player2)
                     (assoc :connections {:conn {:player :player1
                                                 :out :channel}}))]
    (storage/write-room! dir original)
    (let [loaded (storage/read-room dir "ABC123")]
      (is (= "ABC123" (:id loaded)))
      (is (empty? (:connections loaded)))
      (is (false? (get-in loaded [:seats :player1 :connected?])))
      (is (true? (get-in loaded [:seats :player2 :bot?])))
      (is (true? (get-in loaded [:seats :player2 :connected?])))
      (is (some? (:updated-at loaded))))))

(deftest delete-room-test
  (let [dir (.toFile (Files/createTempDirectory "karbosh-storage-delete-test"
                                                (make-array FileAttribute 0)))
        room (room/new-room "ABC123" 9)]
    (storage/write-room! dir room)
    (is (storage/room-exists? dir "ABC123"))
    (is (true? (storage/delete-room! dir "ABC123")))
    (is (not (storage/room-exists? dir "ABC123")))))
