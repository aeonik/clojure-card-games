(ns clojure-card-games.karbosh.logic.rules
  (:require [clojure.core.logic :as l]
            [clojure-card-games.karbosh.shared.rules :as rules]))

(defn same-color-suito [suit other]
  (l/conde
    [(l/== suit :♥) (l/== other :♦)]
    [(l/== suit :♦) (l/== other :♥)]
    [(l/== suit :♠) (l/== other :♣)]
    [(l/== suit :♣) (l/== other :♠)]))

(defn right-bowero [card trump]
  (l/fresh [suit]
    (l/== [:J suit] card)
    (l/== suit trump)))

(defn left-bowero [card trump]
  (l/fresh [suit same-color]
    (l/== [:J suit] card)
    (same-color-suito trump same-color)
    (l/== suit same-color)))

(defn non-left-bowero [card trump]
  (l/fresh [rank suit same-color]
    (l/== [rank suit] card)
    (same-color-suito trump same-color)
    (l/conde
      [(l/!= rank :J)]
      [(l/== rank :J)
       (l/!= suit same-color)])))

(defn effective-suito [card trump suit]
  (l/conde
    [(left-bowero card trump)
     (l/== suit trump)]
    [(non-left-bowero card trump)
     (l/fresh [rank printed-suit]
       (l/== [rank printed-suit] card)
       (l/== suit printed-suit))]))

(defn- succeed-if [x]
  (if x l/succeed l/fail))

(defn empty-tricko [trick]
  (l/project [trick]
    (succeed-if (empty? trick))))

(defn trick-leado [trick trump lead]
  (l/project [trick trump]
    (if-let [suit (rules/trick-lead trick trump)]
      (l/== lead suit)
      l/fail)))

(defn can-follow? [hand lead trump]
  (some #(= lead (rules/effective-suit % trump)) hand))

(defn cannot-followo [hand lead trump]
  (l/project [hand lead trump]
    (succeed-if (not (can-follow? hand lead trump)))))

(defn legal-cardo [hand trick trump card]
  (l/all
    (l/membero card (rules/distinct-cards hand))
    (l/conde
      [(empty-tricko trick)]
      [(l/fresh [lead suit]
         (trick-leado trick trump lead)
         (effective-suito card trump suit)
         (l/== suit lead))]
      [(l/fresh [lead]
         (trick-leado trick trump lead)
         (cannot-followo hand lead trump))])))

(defn legal-cards [hand trick trump]
  (vec (l/run* [card]
         (legal-cardo hand trick trump card))))

(defn winning-playo [trick trump play]
  (l/project [trick trump]
    (if-let [winner (rules/winning-play trick trump)]
      (l/== play winner)
      l/fail)))

(defn winning-play [trick trump]
  (first (l/run 1 [play]
           (winning-playo trick trump play))))

(defn beats-cardo [trump lead challenger incumbent]
  (l/project [trump lead challenger incumbent]
    (succeed-if (rules/beats? trump lead challenger incumbent))))

(defn legal-winning-cards [hand trick trump player]
  (let [plays (for [card (legal-cards hand trick trump)]
                {:player player :card card})]
    (->> plays
         (filter #(= player (:player (rules/winning-play (conj trick %) trump))))
         (mapv :card))))
