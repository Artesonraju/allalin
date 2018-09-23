(ns allalin.position
  (:require
    [allalin.component :as comp]))

(defn inc-position [position]
  (let [{:keys [counts parts]} position
        {:keys [total current stock moves]} counts]
    (cond
      (< stock moves) (update-in position [:counts :stock] inc)
      (< current (dec total)) (update position :counts assoc :stock 0
                                                             :current (inc current)
                                                             :moves (-> parts (nth (inc current)) :moves))
      :else position)))

(defn dec-position [position]
  (let [{:keys [counts parts]} position
        {:keys [current stock]} counts]
    (cond
      (> stock 0) (update-in position [:counts :stock] dec)
      (> current 0) (let [c (dec current)
                          moves (-> parts (nth c) :moves)]
                      (update position :counts assoc :stock moves
                                                     :current c
                                                     :moves moves))
      :else position)))

(defn last-position [position]
  (let [pages (:parts position)
        moves (-> pages last :moves)]
    (update position :counts assoc :current (dec (count pages))
                                   :stock (dec moves)
                                   :moves moves)))

(defn nth-position
  ([index position] (nth-position index 0 position))
  ([index stock position]
   (let [pages (:parts position)
         safe-index (min index (dec (count pages)))
         moves (-> pages (nth safe-index) :moves)
         safe-stock (min stock moves)]
     (update position :counts merge {:current safe-index
                                     :stock stock
                                     :moves moves}))))

(defn adapt-position
  [old-pos new-pos]
  (let [new-total (-> new-pos :counts :total)
        {old-current :current old-stock :stock} (:counts old-pos)
        new-current (min old-current (dec new-total))
        new-moves (-> new-pos :parts (nth new-current) :moves dec)
        new-stock (min old-stock new-moves)]
    (println "stock" new-stock)
    (update new-pos :counts assoc :current new-current
                                  :stock new-stock
                                  :moves new-moves)))

(defn build-position [pages old-position]
  (let [position {:parts (mapv #(comp/build-composed-part (:children %)) pages)
                  :counts {:total (count pages)}}]
    (if old-position
      (adapt-position old-position position)
      (nth-position 0 position))))

(defn init-position
  [config old-position]
  (let [pages (:pages config)]
    (build-position pages old-position)))