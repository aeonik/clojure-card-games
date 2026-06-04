(ns clojure-card-games.io-tui-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.io.tui :as tui]))

(deftest parse-action-test
  (let [game {:phase :trick-playing
              :current-player :player1
              :trump :♠
              :current-trick []
              :players {:player1 {:hand [[:Q :♥]]}}}]
    (is (= {:type :play-card :player :player1 :card [:Q :♥]}
           (dissoc (tui/parse-action game "Qh") :next-seq :input)))))
