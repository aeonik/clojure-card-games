(ns clojure-card-games.karbosh.hiccup-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.hiccup :as h]))

(deftest renders-elements-with-attrs-test
  (is (= "<button class=\"danger active\" type=\"button\" disabled data-room=\"A&amp;B\">Delete</button>"
         (h/render
          [:button {:class ["danger" nil "active"]
                    :type "button"
                    :disabled true
                    :hidden false
                    :data-room "A&B"}
           "Delete"]))))

(deftest renders-fragment-vectors-test
  (is (= "<span>One</span><strong>Two</strong>"
         (h/render [[:span "One"] [:strong "Two"]]))))

(deftest escapes-normal-text-but-not-raw-text-tags-test
  (is (= "<p>&lt;safe&gt;</p>"
         (h/render [:p "<safe>"])))
  (is (= "<style>.trick>div{color:#fff}</style>"
         (h/render [:style ".trick>div{color:#fff}"]))))
