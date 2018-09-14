(ns allalin.utils)

(defn reduce-indexed
  "Reduce with index, expects a function with arity [acc index x]"
  [f init coll]
  (reduce-kv f init (vec coll)))

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn last-index-of [f coll]
  (last (indices-of f coll)))