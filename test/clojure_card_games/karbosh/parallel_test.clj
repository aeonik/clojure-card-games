(ns clojure-card-games.karbosh.parallel-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.parallel :as parallel]))

(deftest mapv-maybe-parallel-test
  (testing "preserves order and returns a vector"
    (is (= [2 4 6]
           (parallel/mapv-maybe-parallel 1 #(* 2 %) [1 2 3])))))

(deftest maybe-parallel-map-test
  (testing "preserves lazy map semantics for callers that stop early"
    (is (= [0 1 4]
           (doall
            (take 3 (parallel/maybe-parallel-map 1 #(* % %) (range))))))))
