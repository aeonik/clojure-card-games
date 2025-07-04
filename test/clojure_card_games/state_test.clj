(ns clojure-card-games.state-test
  (:require [clojure.test :refer :all]
            [clojure-card-games.state :as state]))

(deftest remove-first-test
  (is (= [:a :b]     (state/remove-first :c [:a :b])))
  (is (= [:b :c]     (state/remove-first :a [:a :b :c])))
  (is (= [:a :c :b]  (state/remove-first :b [:a :b :c :b])))
  (is (= [:a :b :c]  (state/remove-first :x [:a :b :c]))))