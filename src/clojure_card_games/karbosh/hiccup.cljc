(ns clojure-card-games.karbosh.hiccup
  (:require [clojure.string :as str]))

(defn- escape-text [value]
  (let [s (str value)]
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;")
        (str/replace "'" "&#39;"))))

(def ^:private raw-text-tags
  #{:script :style})

(defn- normalize-class [class-value]
  (if (sequential? class-value)
    (str/join " " (keep #(when % (str %)) class-value))
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
         (for [[attr-name value] attrs
               :when (not (or (nil? value) (false? value)))]
           (let [attr-name (name attr-name)]
             (cond
               (true? value) (str " " attr-name)
               (= attr-name "class")
               (str " class=\"" (escape-text (normalize-class value)) "\"")
               (= attr-name "style")
               (str " style=\"" (escape-text (normalize-style value)) "\"")
               :else (str " " attr-name "=\"" (escape-text value) "\""))))))

(defn- tag-name [tag]
  (if (keyword? tag)
    (name tag)
    (str tag)))

(declare render*)

(defn- render-element [node]
  (let [raw-tag (first node)
        tail (rest node)
        has-attrs? (map? (first tail))
        attrs (if has-attrs? (first tail) nil)
        children (if has-attrs? (rest tail) tail)
        raw-text? (contains? raw-text-tags raw-tag)]
    (str
     "<" (tag-name raw-tag) (attrs->string attrs) ">"
     (apply str (map #(render* % raw-text?) children))
     "</" (tag-name raw-tag) ">")))

(defn- render* [node raw-text?]
  (cond
    (nil? node) ""
    (and raw-text? (not (sequential? node))) (str node)
    (string? node) (escape-text node)
    (number? node) (str node)
    (boolean? node) (str node)
    (and (vector? node) (keyword? (first node))) (render-element node)
    (sequential? node) (apply str (map #(render* % raw-text?) node))
    :else (escape-text node)))

(defn render
  "Render a Hiccup-style tree into an HTML string."
  [node]
  (render* node false))
