(ns clojure-card-games.probability-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.probability.hypergeom :as hypergeom]
            [clojure-card-games.probability.probability :as probability]))

(deftest hypergeom-test
  (is (= 10 (hypergeom/choose 5 2)))
  (is (= 1 (hypergeom/prob-hg 4 0 4 4))))

(deftest empirical-test
  (let [hero [[:J :♠] [:J :♠] [:A :♠] [:K :♠]
              [:Q :♠] [:A :♦] [10 :♥] [9 :♥]]]
    (is (boolean? (probability/hero-wins-once? hero (java.util.Random. 1)))))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"not available"
       (probability/hero-wins-once? [[:J :♠] [:J :♠] [:J :♠]]
                                    (java.util.Random. 1)))))
