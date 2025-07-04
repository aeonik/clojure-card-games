(ns clojure-card-games.rules
  (:require [clojure-card-games.cards :as c]))

(def multipliers {:trump 100 :lead 10 :off 1})
(def bonuses     {:right-bower 2000 :left-bower 900})

(defn effective-suit [[rank suit] trump]
  (if (and (= rank :J)
           (case trump
             :♥ (= suit :♦)
             :♦ (= suit :♥)
             :♠ (= suit :♣)
             :♣ (= suit :♠)
             false))
    trump
    suit))

(defn bower? [[rank suit] trump]
  (and (= rank :J)
       (case trump
         :♥ (= suit :♦)
         :♦ (= suit :♥)
         :♠ (= suit :♣)
         :♣ (= suit :♠)
         false)))

(defn card-value [card trump lead]
  (let [[rank suit] card
        eff-suit (effective-suit card trump)
        base     (get {:A 8 :K 7 :Q 6 :J 5 10 4 9 3} rank 0)
        right-bower? (and (= rank :J) (= suit trump))
        left-bower?  (bower? card trump)]
    (cond
      right-bower?                        (:right-bower bonuses)
      left-bower?                         (:left-bower  bonuses)
      (= eff-suit trump)                  (* base (:trump multipliers))
      (and lead (= eff-suit lead))        (* base (:lead multipliers))
      :else                               (* base (:off multipliers)))))

(defn resolve-trick [trick trump]
  (let [lead-suit (-> trick first :card second)]
    (->> trick
         (sort-by (fn [{:keys [card]}]
                    (card-value card trump lead-suit))
                  >)
         first
         :player)))

(defn valid-bid? [value]
  (and (number? value) (<= 1 value 8)))