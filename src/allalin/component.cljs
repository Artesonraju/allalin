(ns allalin.component
  (:require [cljs.spec.alpha :as s]
            [rum.core :as rum]))

(defn merge-styles [default custom]
  "Special treatment for top, left, right and bottom properties"
  (merge (cond-> default
           (:top custom) (dissoc :bottom)
           (:bottom custom) (dissoc :top)
           (:left custom) (dissoc :right)
           (:right custom) (dissoc :left))
         custom))

(defn default-component
  [keyword this default]
  (let [d (get default keyword)
        style (merge-styles (:style d) (:style this))]
    (-> (merge d this)
        (assoc :style style))))

(def transforms {:italic [:font-style "italic"]
                 :bold [:font-weight "bold"]
                 :underlined [:text-decoration "underline"]
                 :line-through [:text-decoration "line-through"]
                 :uppercase [:text-transform "uppercase"]})

; display decorated text
(defn transform-to-style
  [transform]
  (->> (if (keyword? transform) [transform] transform)
       (map (partial get transforms))
       (into {})))

(defn text-content
  [index text actions]
  (let [{:keys [string link transform]} text
        tag (if (some? link) :a :span)
        href (when (string? link) link)
        on-click (when (number? link) (partial (:inner-link actions) link))
        style (transform-to-style transform)
        attrs (cond-> {:key index}
                (some? href) (assoc :href href
                                    :target "_blank"
                                    :rel "noreferrer noopener")
                (some? style) (assoc :style style)
                (some? on-click) (assoc :on-click on-click))]
    [tag attrs string]))

(defn text-contents
  [texts actions]
  (->> (if (or (string? texts) (map? texts)) [texts] texts)
       (map-indexed (fn [index content]
                      (if (not (map? content))
                        content
                        (text-content index content actions))))))

(defprotocol IComponent
  (add-default [this default])
  (build-position [this])
  (images [this])
  (render [this paging props]))

(defn render-children
  "Render a list of components"
  [components pagings props]
  (map-indexed
    (fn [index comp]
      (rum/with-key
        (render comp (nth pagings index) props)
        (str index)))
    components))

; default implementations
(extend-type object
  IComponent
  (build-position [_]
    {:length 1})
  (images [this]
    (mapcat images (:children this))))

; title
(rum/defc title < rum/static
  [this props]
  (let [{:keys [level content style]} this
        tag (keyword (str "h" level))]
    [tag {:style style}
     (text-contents content (:actions props))]))

;only required attributes in record declaration
(defrecord Title [content]
  IComponent
  (add-default [this default]
    (default-component :title this default))
  (render [this _ props]
    (title (add-default this (:default props)) props)))

(extend-protocol IPrintWithWriter
  Title
  (-pr-writer [this writer _]
    (write-all writer "#title" (into {} this))))

; text
(rum/defc text < rum/static
  [this props]
  (let [{:keys [content style]} this]
    [:div.text {:style style}
     (text-contents content (:actions props))]))

(defrecord Text [content]
  IComponent
  (add-default [this default]
    (default-component :text this default))
  (render [this _ props]
    (text (add-default this (:default props)) props)))

(extend-protocol IPrintWithWriter
  Text
  (-pr-writer [this writer _]
    (write-all writer "#text" (into {} this))))

; image
(rum/defc image < rum/static
  [this]
  (let [{:keys [src style]} this
        s (str "./images/" src)]
    [:img {:src s
           :style style}]))

(defrecord Image [src]
  IComponent
  (add-default [this default]
    (default-component :image this default))
  (render [this _ props] (image (add-default this (:default props))))
  (images [this] (vector (:src this))))

(extend-protocol IPrintWithWriter
  Image
  (-pr-writer [this writer _]
    (write-all writer "#image" (into {} this))))

; page-number
(rum/defc page-number < rum/static
  [this props]
  (let [{:keys [total? style]} this
        {:keys [current total]} (:counts props)]
    [:div.page-number {:style style}
     (str (inc current) (when total? (str "/" total)))]))

(defrecord PageNumber [total?]
  IComponent
  (add-default [this default]
    (default-component :page-number this default))
  (render [this _ props]
    (page-number (add-default this (:default props)) props)))

(extend-protocol IPrintWithWriter
  PageNumber
  (-pr-writer [this writer _]
    (write-all writer "#pagenumber" (into {} this))))

;code
(rum/defc code < rum/static
  [this props]
  (let [{:keys [content style]} this]
    [:pre.code {:style style}
     [:code
      (text-contents content (:actions props))]]))

(defrecord Code [content]
  IComponent
  (add-default [this default]
    (default-component :code this default))
  (render [this _ props]
    (code (add-default this (:default props)) props)))

(extend-protocol IPrintWithWriter
  Code
  (-pr-writer [this writer _]
    (write-all writer "#code" (into {} this))))

; list
(rum/defc liste < rum/static
  [this paging props]
  (let [{:keys [type points fragmented? style]} this
        displayed (if fragmented?
                    (take (inc (:current paging)) points)
                    points)
        tag (if (= :bullet type) :ul :ol)]
    [tag {:style style}
     (map-indexed (fn [index content]
                    [:li {:key index} (text-contents content (:actions props))])
                  displayed)]))

(defrecord Liste [type points]
  IComponent
  (add-default [this default]
    (default-component :liste this default))
  (render [this paging props]
    (liste (add-default this (:default props)) paging props))
  (build-position [this]
    {:length (if (:fragmented? this) (count points) 1)}))

(extend-protocol IPrintWithWriter
  Liste
  (-pr-writer [this writer _]
    (write-all writer "#liste" (into {} this))))

;section
(rum/defc section < rum/static
  [this paging props]
  (let [{:keys [children style]} this]
    [:div.section {:style style}
     (render-children children (:children paging) props)]))

(defrecord Section [children]
  IComponent
  (render [this paging props]
    (section (add-default this (:default props)) paging props))
  (add-default [this default]
    (default-component :section this default))
  (build-position [_]
    {:length (count children)
     :children (mapv build-position children)}))

(extend-protocol IPrintWithWriter
  Section
  (-pr-writer [this writer _]
    (write-all writer "#section" (into {} this))))

;fragments
(rum/defc fragments < rum/static
  [this paging props]
  (let [{:keys [children]} this
        index (inc (:current paging))
        displayed (take index children)]
    (render-children displayed (:children paging) props)))

(defrecord Fragments [children]
  IComponent
  (render [this paging props]
    (fragments (add-default this (:default props)) paging props))
  (add-default [this default]
    (default-component :fragments this default))
  (build-position [_]
    {:length (count children)
     :children (mapv build-position children)}))

(extend-protocol IPrintWithWriter
  Fragments
  (-pr-writer [this writer _]
    (write-all writer "#fragments" (into {} this))))

;steps
(rum/defc steps < rum/static
  [this paging props]
  (let [{:keys [children]} this
        index (:current paging)
        step (nth children index)
        step-paging (nth children index)]
     (render step step-paging props)))

(defrecord Steps [children]
  IComponent
  (render [this paging props]
    (steps (add-default this (:default props)) paging props))
  (add-default [this default]
    (default-component :steps this default))
  (build-position [_]
    {:length (count children)
     :children (mapv build-position children)}))

(extend-protocol IPrintWithWriter
  Steps
  (-pr-writer [this writer _]
    (write-all writer "#steps" (into {} this))))

(def edn-readers {'title map->Title
                  'text map->Text
                  'image map->Image
                  'code map->Code
                  'pagenumber map->PageNumber
                  'liste map->Liste
                  'section map->Section
                  'fragments map->Fragments
                  'steps map->Steps})

; specs
(def liste-types #{:numbered :bullet})

(s/def ::transforms (s/+ (into #{} (keys transforms))))
(s/def ::string string?)
(s/def ::tranformed-string (s/keys :req-un [::string]
                                   :opt-un [::transform
                                            ::link]))

(s/def ::text (s/or :string ::string
                    :tranformed ::tranformed-string))
(s/def ::content (s/or :string ::text
                       :strings (s/coll-of ::text)))

(s/def ::style map?)

(defmulti component-spec type)

(s/def ::level (s/and pos-int? #(<= % 5)))

(defmethod component-spec Title [_]
  (s/keys :opt-un [::level ::content ::style]))

(defmethod component-spec Text [_]
  (s/keys :opt-un [::content ::style]))

(s/def ::src string?)

(defmethod component-spec Image [_]
  (s/keys :opt-un [::src ::style]))

(s/def ::total? boolean?)

(defmethod component-spec PageNumber [_]
  (s/keys :opt-un [::total? ::style]))

(defmethod component-spec Code [_]
  (s/keys :opt-un [::content ::style]))

(s/def ::type liste-types)

(s/def ::contents (s/coll-of ::content))

(s/def ::fragmented? boolean?)

(s/def ::points (s/coll-of ::content))

(defmethod component-spec Liste [_]
  (s/keys :opt-un [::type ::points ::style ::fragmented?]))

(defmethod component-spec Section [_]
  (s/keys :opt-un [::children ::style]))

(defmethod component-spec Fragments [_]
  (s/keys :opt-un [::children ::style]))

(defmethod component-spec Steps [_]
  (s/keys :opt-un [::children ::style]))

(s/def ::component
  (s/multi-spec component-spec type))