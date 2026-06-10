(ns clojure-card-games.karbosh.page-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure-card-games.karbosh.page :as page]))

(deftest html-escapes-text-and-attributes-test
  (is (= "<p>&lt;safe&gt;</p>"
         (page/html [:p "<safe>"])))
  (is (= "<input value=\"x&quot; onmouseover=&quot;alert(1)\">"
         (page/html [:input {:value "x\" onmouseover=\"alert(1)"}]))))

(deftest html-renders-attrs-and-fragments-test
  (is (= "<button class=\"danger active\" data-room=\"A&amp;B\" disabled type=\"button\">Delete</button>"
         (page/html
          [:button {:class ["danger" "active"]
                    :type "button"
                    :disabled true
                    :hidden false
                    :data-room "A&B"}
           "Delete"])))
  (is (= "<span>One</span><strong>Two</strong>"
         (page/html (list [:span "One"] [:strong "Two"])))))

(deftest render-builds-full-document-test
  (let [doc (page/render {:title "T" :stylesheets ["admin.css" "snapshot.css"]}
                         [:main "Body"])]
    (is (str/starts-with? doc "<!doctype html>"))
    (is (str/includes? doc "<title>T</title>"))
    (is (str/includes? doc "<link href=\"/karbosh/assets/css/admin.css\" rel=\"stylesheet\">"))
    (is (str/includes? doc "<link href=\"/karbosh/assets/css/snapshot.css\" rel=\"stylesheet\">"))
    (is (str/includes? doc "<body><main>Body</main></body>"))))
