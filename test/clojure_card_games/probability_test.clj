(ns clojure-card-games.probability-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.probability.hypergeom :as hypergeom]
            [clojure-card-games.probability.probability :as probability]))

(deftest hypergeom-test
  (is (= 10 (hypergeom/choose 5 2)))
  (is (= 1 (hypergeom/prob-hg 4 0 4 4)))
  (is (= 1 (hypergeom/prob-labeled-hands-min-success 3 0 1 3 1)))
  (is (= 0 (hypergeom/prob-labeled-hands-min-success 2 1 1 3 1)))
  (is (= 1 (hypergeom/prob-opponents-follow-suit 40)))
  (is (= 0 (hypergeom/prob-opponents-follow-suit 0)))
  (is (= 1 (hypergeom/prob-void-and-trump 0 40))))

(deftest empirical-test
  (let [hero [[:J :♠] [:J :♠] [:A :♠] [:K :♠]
              [:Q :♠] [:A :♦] [10 :♥] [9 :♥]]]
    (is (boolean? (probability/hero-wins-once? hero (java.util.Random. 1)))))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"not available"
       (probability/hero-wins-once? [[:J :♠] [:J :♠] [:J :♠]]
                                    (java.util.Random. 1)))))
