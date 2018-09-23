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
  (build-part [this])
  (images [this])
  (render [this point props]))

; default implementations
(extend-type object
  IComponent
  (build-part [_]
    {:moves 0})
  (images [this]
    (mapcat images (:children this))))

(defn render-children
  [components point props]
  (-> (reduce
        (fn [[acc parts stock index] comp]
          (let [{:keys [moves] :as part} (first parts)
                new-stock (max 0 (- stock moves))
                current (min stock moves)]
            [(conj acc (rum/with-key
                         (render comp {:part part :stock current} props)
                         (str index)))
             (rest parts)
             new-stock
             (inc index)]))
        [[] (-> point :part :children) (:stock point) 0]
        components)
      (first)))

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
  [this point props]
  (let [stock (:stock point)
        {:keys [type points fragmented? style]} this
        displayed (if fragmented?
                    (take (inc stock) points)
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
  (render [this point props]
    (liste (add-default this (:default props)) point props))
  (build-part [this]
    {:moves (if (:fragmented? this) (dec (count points)) 0)}))

(extend-protocol IPrintWithWriter
  Liste
  (-pr-writer [this writer _]
    (write-all writer "#liste" (into {} this))))

;section
(rum/defc section < rum/static
  [this point props]
  (let [{:keys [children style]} this]
    [:div.section {:style style}
     (render-children children point props)]))

(defn build-composed-part
  ([children] (build-composed-part children 0))
  ([children init]
   (let [processed (reduce (fn [part child]
                             (let [child-part (build-part child)]
                               (-> part
                                   (update :children conj child-part)
                                   (update :moves + (:moves child-part)))))
                           {:moves init
                            :children []}
                           children)]
     (if (> (:moves processed) init)
       processed
       {:moves init}))))

(defrecord Section [children]
  IComponent
  (render [this point props]
    (section (add-default this (:default props)) point props))
  (add-default [this default]
    (default-component :section this default))
  (build-part [_]
    (build-composed-part children)))

(extend-protocol IPrintWithWriter
  Section
  (-pr-writer [this writer _]
    (write-all writer "#section" (into {} this))))

;fragments
(defn render-fragment-children
  [components point props]
  (-> (reduce
        (fn [[acc parts stock index] comp]
          (let [{:keys [moves] :as part} (first parts)
                moves (if (pos? index) (inc moves) (or moves 0))
                new-stock (max 0 (- stock moves))
                current (min stock moves)
                new-acc (conj acc (rum/with-key
                                    (render comp {:part part :stock current} props)
                                    (str index)))]
            (if (= new-stock 0)
              (reduced [new-acc])
              [new-acc
               (rest parts)
               new-stock
               (inc index)])))
        [[] (-> point :part :children) (:stock point) 0]
        components)
      (first)))

(rum/defc fragments < rum/static
  [this point props]
  (render-fragment-children (:children this) point props))

(defrecord Fragments [children]
  IComponent
  (render [this point props]
    (fragments (add-default this (:default props)) point props))
  (add-default [this default]
    (default-component :fragments this default))
  (build-part [_]
    (build-composed-part children (dec (count children)))))

(extend-protocol IPrintWithWriter
  Fragments
  (-pr-writer [this writer _]
    (write-all writer "#fragments" (into {} this))))

;steps
(defn render-steps-children
  [components point props]
  (-> (reduce
        (fn [[_ parts stock index] comp]
          (let [{:keys [moves] :as part} (first parts)
                moves (if (pos? index) (inc moves) (or moves 0))
                new-stock (max 0 (- stock moves))
                current (min stock moves)]
            (if (= new-stock 0)
              (reduced [(render comp {:part part :stock current} props)])
              [nil
               (rest parts)
               new-stock
               (inc index)])))
        [nil (-> point :part :children) (:stock point) 0]
        components)
      (first)))

(rum/defc steps < rum/static
  [this point props]
  (render-steps-children (:children this) point props))

(defrecord Steps [children]
  IComponent
  (render [this paging props]
    (steps (add-default this (:default props)) paging props))
  (add-default [this default]
    (default-component :steps this default))
  (build-part [_]
    (build-composed-part children (dec (count children)))))

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

(s/def ::children (s/coll-of ::component :min-count 1))

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