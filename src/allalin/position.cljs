(ns allalin.position
  (:require
    [allalin.component :as comp]
    [allalin.utils :refer [reduce-indexed]]))

(defn to-next-part
  [parts]
  (reduce-indexed
    (fn [acc index comp]
      (let [{:keys [current children length]} comp
            next-children (to-next-part children)]
        (cond
          (not= next-children children) (reduced (assoc-in acc [index :children] next-children))
          (< current (dec length)) (reduced (update-in acc [index :current] inc))
          :else acc)))
    parts
    parts))

(defn to-prev-part
  [parts]
  (reduce-indexed
    (fn [acc rev-index comp]
      (let [index (- (dec (count parts)) rev-index)
            {:keys [current children]} comp
            next-children (to-prev-part children)]
        (cond
          (not= next-children children) (reduced (assoc-in acc [index :children] next-children))
          (> current 0) (reduced (update-in acc [index :current] dec))
          :else acc)))
    parts
    (reverse parts)))

(defn to-last-part [p]
    (-> p
        (assoc :current (dec (:length p)))
        (update :children #(mapv to-last-part %))))

(defn to-first-part [p]
  (-> p
      (assoc :current 0)
      (update :children #(mapv to-first-part %))))

(defn inc-position [position]
  (let [{:keys [counts paging]} position
        current (:current counts)
        parts (nth paging current)
        new-parts (to-next-part parts)]
    (if (not= new-parts parts)
      (assoc-in position [:paging current] new-parts)
      (if (= current (dec (count paging)))
        position
        (update-in position [:counts :current] inc)))))

(defn dec-position [position]
  (let [{:keys [counts paging]} position
        current (:current counts)
        parts (nth paging current)
        new-parts (to-prev-part parts)]
    (if (not= new-parts parts)
      (assoc-in position [:paging current] new-parts)
      (if (= current 0)
        position
        (update-in position [:counts :current] dec)))))

(defn last-position [position]
  (let [pages (:paging position)]
    {:counts {:current (dec (count pages))
              :total (count pages)}
     :paging (mapv #(mapv to-last-part %) pages)}))

(defn nth-page-position [index position]
  (let [pages (:paging position)
        safe-index (min index (dec (count pages)))]
    {:counts {:current index
              :total (count pages)}
     :paging (->> pages
                 (map-indexed
                   (fn [i cs]
                     (if (< i safe-index)
                       (mapv to-last-part cs)
                       (mapv to-first-part cs))))
                 (into []))}))

(defn adapt-parts
  [old-parts new-parts]
  (reduce-indexed (fn [acc index new-part]
                    (let [old-part (nth old-parts index)
                          {old-current :current old-children :children} old-part
                          {new-length :length} old-part]
                      (if (or (nil? old-part)
                              (>= index (count new-parts)))
                        (conj acc (to-first-part new-part))
                        (conj acc (-> new-part
                                      (assoc :current (min old-current new-length))
                                      (update :children #(adapt-parts old-children %)))))))
                  []
                  new-parts))

(defn adapt-pagings
  [paging old-current old-parts]
  (into [] (map-indexed (fn [index parts]
                          (cond
                            (= index old-current) (adapt-parts old-parts parts)
                            (< index old-current) (mapv to-last-part parts)
                            (> index old-current) (mapv to-first-part parts)))
                        paging)))

(defn adapt-position
  [old-pos new-pos]
  (let [new-total (-> new-pos :counts :total)
        old-current (-> old-pos :counts :current)
        old-parts (get-in old-pos [:paging old-current])]
    (-> new-pos
        (assoc-in [:counts :current] (min old-current (dec new-total)))
        (update :paging #(adapt-pagings % old-current old-parts)))))

(defn build-position [pages old-position]
  (let [position {:paging (mapv (fn [page]
                                  (mapv comp/build-position
                                        (:children page)))
                                pages)
                  :counts {:total (count pages)}}]
    (if old-position
      (adapt-position old-position position)
      (nth-page-position 0 position))))

(defn init-position
  [config old-position]
  (let [pages (:pages config)]
    (build-position pages old-position)))

(defn with-all-pages [position]
  (assoc position :all-paging
    (mapv (fn [parts]
            (mapv to-last-part parts))
          (:paging position))))