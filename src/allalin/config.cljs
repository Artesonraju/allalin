(ns allalin.config
  (:require [cljs.spec.alpha :as s]
            [allalin.utils :refer [reduce-indexed]]
            [allalin.component :as comp]))

(s/def ::children (s/coll-of ::comp/component))

(s/def ::color string?)
(s/def ::width #(or (pos? %) (zero? %)))
(s/def ::height #(or (pos? %) (zero? %)))
(s/def ::background-color string?)
(s/def ::background-position string?)
(s/def ::background-image string?)

(def direction #{:top :left :right :bottom})
(s/def ::header-left direction)
(s/def ::header-right direction)
(s/def ::footer-left direction)
(s/def ::footer-right direction)

(s/def ::around (s/keys :opt-un [::contents
                                 ::color
                                 ::width
                                 ::height
                                 ::background-color
                                 ::background-position
                                 ::background-image
                                 ::header-left
                                 ::header-right
                                 ::footer-left
                                 ::footer-right]))

(s/def ::header ::around)
(s/def ::footer ::around)
(s/def ::left ::around)
(s/def ::right ::around)

(s/def ::page (s/keys :opt-un [::children
                               ::color
                               ::background-color
                               ::background-position
                               ::background-image]))
(s/def ::pages (s/coll-of ::page :min-count 1))

(s/def ::side direction)
(s/def ::color string?)

(s/def ::progress-bar
  (s/keys :req-un [::side ::width ::color]))

(s/def ::title string?)
(s/def ::screen-ratio (s/and number? #(> % 0) #(<= % 1)))
(s/def ::disable-reaload boolean?)

(s/def ::config
  (s/keys :req-un [::pages
                   ::screen-ratio
                   ::title
                   ::header-left
                   ::header-right
                   ::footer-left
                   ::footer-right
                   ::header
                   ::footer
                   ::left
                   ::right]
          :opt-un [::progress-bar
                   ::disable-reaload
                   ::background-image
                   ::background-color
                   ::background-position]))

(defn mode [config old]
  (or (:mode old) (:initial-mode config) :basic))

(defn valid [config]
  (s/valid? ::config config))

(defn problems [config]
  (->> config
       (s/explain-data ::config)
       (:cljs.spec.alpha/problems)))

(defn foreground-images
  [components]
  (mapcat comp/images components))

(defn background-images
  [config]
  (->> (conj (:pages config) config)
       (mapcat (fn [x]
                 (conj (map #(get-in x [% :background-image])
                            [:header :left :footer :right])
                       (get x :background-image))))
       (filterv some?)))

(defn images
  [config]
  (->> (:pages config)
       (mapcat (juxt :header :footer :left :right identity))
       (filter some?)
       (mapcat :children)
       (foreground-images)
       (filter some?)
       (concat (background-images config))
       (distinct)))