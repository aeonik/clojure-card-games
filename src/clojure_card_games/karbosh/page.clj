(ns clojure-card-games.karbosh.page
  "Shared HTML page layout for server-rendered pages, rendered with hiccup."
  (:require [hiccup2.core :as h]))

(defn stylesheet [basename]
  [:link {:rel "stylesheet" :href (str "/karbosh/assets/css/" basename)}])

(defn html
  "Render a hiccup fragment to an HTML string."
  [content]
  (str (h/html {:mode :html} content)))

(defn render
  "Render a complete HTML document string with the shared head boilerplate."
  [{:keys [title stylesheets]} & body]
  (str "<!doctype html>"
       (h/html {:mode :html}
               [:html {:lang "en"}
                [:head
                 [:meta {:charset "utf-8"}]
                 [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
                 [:title title]
                 (map stylesheet stylesheets)]
                (into [:body] body)])))
