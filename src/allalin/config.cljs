(ns allalin.config
  (:require [cljs.spec.alpha :as s]
            [allalin.component :as comp]))

(s/def ::children (s/coll-of ::comp/component))

(s/def ::color string?)
(s/def ::width #(or (pos? %) (zero? %)))
(s/def ::height #(or (pos? %) (zero? %)))
(s/def ::background-color string?)
(s/def ::background-position string?)
(s/def ::background-image string?)

(def around #{:header :left :right :footer})
(s/def ::header-left around)
(s/def ::header-right around)
(s/def ::footer-left around)
(s/def ::footer-right around)

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

(def side #{:top :left :right :bottom})
(s/def ::side side)
(s/def ::color string?)

(s/def ::progress-bar
  (s/keys :req-un [::side ::width ::color]))

(s/def ::title string?)
(s/def ::screen-ratio (s/and number? #(> % 0) #(<= % 1)))
(s/def ::disable-edit boolean?)
(s/def ::disable-print boolean?)
(s/def ::disable-notes boolean?)

(s/def ::default map?)

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
                   ::right
                   ::default]
          :opt-un [::progress-bar
                   ::disable-edit
                   ::disable-notes
                   ::disable-print
                   ::background-image
                   ::background-color
                   ::background-position]))

(defn mode [old-mode]
  (or old-mode :basic))

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