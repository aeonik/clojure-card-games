(ns clojure-card-games.karbosh.parallel)

(def default-threshold 8)

(defn multicore? []
  (> (.availableProcessors (Runtime/getRuntime)) 1))

(defn pmapv
  "Eager, order-preserving `pmap`."
  [f coll]
  (->> coll
       (pmap f)
       (doall)
       (vec)))

(defn mapv-maybe-parallel
  "Use `pmap` only when the work batch is large enough to pay for futures."
  ([f coll]
   (mapv-maybe-parallel default-threshold f coll))
  ([threshold f coll]
   (let [items (vec coll)]
     (if (and (multicore?)
              (>= (count items) threshold))
       (pmapv f items)
       (mapv f items)))))

(defn maybe-parallel-map
  "Lazy sequential map for small work, lazy `pmap` for large work."
  [threshold f coll]
  (if (and (multicore?)
           (>= (bounded-count threshold coll) threshold))
    (pmap f coll)
    (map f coll)))
