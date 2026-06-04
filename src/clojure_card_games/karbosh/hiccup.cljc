(ns clojure-card-games.karbosh.hiccup
  (:require [clojure.string :as str]))

(defn- escape-text [value]
  (let [s (str value)]
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;")
        (str/replace "'" "&#39;") )))

(defn- normalize-class [class-value]
  (if (sequential? class-value)
    (str/join " " (map str class-value))
    (str class-value)))

(defn- normalize-style [style]
  (cond
    (map? style)
    (str/join "; " (for [[k v] style]
                      (str (name k) ": " v)))

    :else
    (str style)))

(defn- attrs->string [attrs]
  (apply str
         (for [[name value] attrs
               :when (not (or (nil? value) (false? value)))]
           (let [name (name name)]
             (cond
               (true? value) (str " " name)
               (= name "class") (str " class=\"" (escape-text (normalize-class value)) "\"")
               (= name "style") (str " style=\"" (escape-text (normalize-style value)) "\"")
               :else (str " " name "=\"" (escape-text value) "\""))))))

(defn- tag-name [tag]
  (if (keyword? tag)
    (name tag)
    (str tag)))

(defn render
  "Render a Hiccup-style tree into an HTML string."
  [node]
  (cond
    (nil? node) ""
    (string? node) (escape-text node)
    (number? node) (str node)
    (boolean? node) (str node)
    (vector? node)
    (let [raw-tag (first node)
          rest (rest node)
          has-attrs? (map? (first rest))
          attrs (if has-attrs? (first rest) nil)
          children (if has-attrs? (rest rest) rest)]
      (str
       "<" (tag-name raw-tag) (attrs->string attrs) ">"
       (apply str (map render children))
       "</" (tag-name raw-tag) ">"))
    (sequential? node)
    (apply str (map render node))
    :else (escape-text node)))
