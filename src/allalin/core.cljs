(ns allalin.core
  (:require [rum.core :as rum]
            [clojure.string :as string]
            [cljs.reader :refer [read-string]]
            [cljs.spec.alpha :as s]))

(enable-console-print!)

; default values

(def default-config {:screen-ratio 0.625
                     :color "#111"
                     :background-color "#eee"
                     :background-position "center"
                     :title "Allalin - Presentation framework"
                     :default {:title {:level 2
                                       :top "5%"
                                       :left "5%"}
                               :image {:top "5%"
                                       :left "5%"}
                               :text {:top "5%"
                                      :left "5%"}
                               :code {:top "5%"
                                      :left "5%"}
                               :list {:top "5%"
                                      :left "5%"}
                               :page-number {:top "5%"
                                             :left "5%"}
                               :section {:top "5%"
                                         :left "5%"}}
                     :text {:top "20%"
                            :left "5%"}

                     :header {:background-color "inherit"
                              :color "inherit"
                              :height 0
                              :content []}
                     :footer {:background-color "inherit"
                              :color "inherit"
                              :height 0
                              :content []}
                     :left {:background-color "inherit"
                            :color "inherit"
                            :width 0
                            :content []}
                     :right {:background-color "inherit"
                             :color "inherit"
                             :width 0
                             :content []}})

; constants
(def available-page-number-types #{:partial :total})
(def available-string-transforms #{:italic :bold :underlined :line-through :uppercase})
(def available-list-types #{:numbered :bullet})
(def simple-components #{:title :image :text :code :page-number})
(def complex-components #{:section :list :fragments :steps})
(def all-components (into simple-components complex-components))

; helpers

(defn reduce-indexed
  "Reduce with index, expects a function with arity [acc index x]"
  [f init coll]
  (reduce-kv f init (vec coll)))

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn last-index-of [f coll]
  (last (indices-of f coll)))

; utils

(defn component-type [content]
  (some all-components (keys content)))

(defn or-config
  ([config keys] (get-in config keys (get-in default-config keys)))
  ([page config keys] (get-in page keys (or-config config keys))))

(defn or-default
  [content config element]
  (let [default (merge
                  (get-in default-config [:default element])
                  (get-in config [:default element]))]
    (cond-> default
      (some? (:top content)) (dissoc :bottom)
      (some? (:bottom content)) (dissoc :top)
      (some? (:left content)) (dissoc :right)
      (some? (:right content)) (dissoc :left)
      true (merge content))))

; config spec

(defmulti content-spec component-type)

(s/def ::title string?)
(s/def ::level (s/and pos-int? #(<= % 5)))
(defmethod content-spec :title [_]
  (s/keys :req-un [::title] :opt-un [::level]))

(s/def ::image string?)
(defmethod content-spec :image [_]
  (s/keys :req-un [::image]))

(s/def :string/string string?)
(s/def :string/transform (s/+ available-string-transforms))
(s/def :string/string (s/or :string string?
                            :number number?))
(s/def ::string (s/keys :req-un [:string/string]
                        :opt-un [:string/transform
                                 :string/link]))

(s/def :text/text (s/or :string string?
                        :strings ::string))
(s/def ::text (s/or :string :text/text
                    :strings (s/coll-of :text/text)))

(defmethod content-spec :text [_]
  (s/keys :req-un [::text]))

(s/def ::code ::text)
(defmethod content-spec :code [_]
  (s/keys :req-un [::code]))

(s/def ::page-number available-page-number-types)
(defmethod content-spec :page-number [_]
  (s/keys :req-un [::page-number]))


(s/def ::list available-list-types)
(s/def :list/fragmented boolean?)
(s/def :list/contents (s/coll-of ::text))
(defmethod content-spec :list [_]
  (s/keys :req-un [::list  :list/contents]
          :opt-un [:list/fragmented]))

(s/def ::section (s/coll-of ::content))
(defmethod content-spec :section [_]
  (s/keys :req-un [::section]))

(s/def ::fragments (s/coll-of ::content :min-count 2))
(defmethod content-spec :fragments [_]
  (s/keys :req-un [::fragments]))

(s/def ::steps (s/coll-of ::content :min-count 2))
(defmethod content-spec :steps [_]
  (s/keys :req-un [::steps]))

(s/def ::content
  (s/multi-spec content-spec :event/type))

(s/def ::contents (s/coll-of ::content))

(s/def ::color string?)
(s/def ::width pos-int?)
(s/def ::height pos-int?)
(s/def ::background-color string?)
(s/def ::background-position string?)
(s/def ::background-image string?)

(s/def ::around (s/keys :opt-un [::contents
                                 ::color
                                 ::width
                                 ::height
                                 ::background-color
                                 ::background-position
                                 ::background-image]))

(s/def :config/header ::around)
(s/def :config/footer ::around)
(s/def :config/left ::around)
(s/def :config/right ::around)

(s/def ::page (s/keys :opt-un [::contents
                               ::color
                               ::background-color
                               ::background-position
                               ::background-image]))
(s/def :config/pages (s/coll-of ::page))

(s/def :config/title string?)
(s/def :config/screen-ratio (s/and number? #(> % 0) #(<= % 1)))
(s/def :config/default (s/every-kv all-components map?))

(s/def ::config
  (s/keys :req-un [:config/pages]
          :opt-un [:config/screen-ratio
                   ::background-color
                   ::background-position
                   ::background-image
                   :config/title
                   :config/default
                   :config/header
                   :config/footer
                   :config/left
                   :config/right
                   :config/pages]))

; state

(defonce app-state (atom {:phase :loading
                          :current 0}))

; page and parts management

(defn is-complex-component
  [content]
  (some #(contains? content %) complex-components))

(defn is-changing-component
  [content]
  (or (some #(contains? content %) #{:fragments :steps})
      (and (contains? content :list) (contains? content :fragmented))))

(defn content-children
  [content]
  (or (:fragments content)
      (:steps content)
      (:section content)
      (:contents content))) ; for list fragment

(defn build-parts
  [contents]
  (mapv (fn [content]
          (when (is-complex-component content)
            (let [children (content-children content)]
              {:children (build-parts children)
               :length (if (is-changing-component content)
                         (count children)
                         1)})))
        contents))

(defn to-first-part
  [parts]
  (mapv (fn [part]
          (when (some? part)
            (-> part
                (update :children to-first-part)
                (assoc :current 0))))
        parts))

(defn to-last-part
  [parts]
  (mapv (fn [part]
          (when (some? part)
            (-> part
                (update :children to-last-part)
                (assoc :current (- (:length part) 1)))))
        parts))

(defn to-next-part
  [parts]
  (reduce-indexed
    (fn [acc index part]
      (let [{:keys [current length children]} part
            next-children (to-next-part children)]
        (cond
          (nil? part) acc
          (not= next-children children) (reduced (assoc-in acc [index :children] next-children))
          (< current (dec length)) (reduced (update-in acc [index :current] inc))
          :else acc)))
    parts
    parts))

(defn to-prev-part
  [parts]
  (reduce-indexed
    (fn [acc rev-index part]
      (let [index (- (dec (count parts)) rev-index)
            {:keys [current children]} part
            next-children (to-prev-part children)]
        (cond
          (nil? part) acc
          (not= next-children children) (reduced (assoc-in acc [index :children] next-children))
          (> current 0) (reduced (update-in acc [index :current] dec))
          :else acc)))
    parts
    (reverse parts)))

(defn nth-page-parts [index pages]
  (let [safe-index (min index (dec (count pages)))
        page (nth pages safe-index)]
    {:current safe-index
     :parts (to-first-part (build-parts (:contents page)))}))

(defn last-page-parts [pages]
  (let [page (last pages)]
    {:current (dec (count pages))
     :parts (to-last-part (build-parts (:contents page)))}))

(defn inc-page-parts [pages current parts]
  (let [next-parts (to-next-part parts)]
    (if (not= next-parts parts)
      {:current current
       :parts next-parts}
      (let [index (min (inc current) (dec (count pages)))
            page (nth pages index)
            new-parts (if (= current index)
                        parts
                        (to-first-part (build-parts (:contents page))))]
        {:current index
         :parts new-parts}))))

(defn dec-page-parts [pages current parts]
  (let [prev-parts (to-prev-part parts)]
    (if (not= prev-parts parts)
      {:current current
       :parts prev-parts}
      (let [index (max (dec current) 0)
            page (nth pages index)
            new-parts (if (= current index)
                        parts
                        (to-last-part (build-parts (:contents page))))]
        {:current index
         :parts new-parts}))))

; interactions

(defn background-images
  [config]
  (->> (conj (:pages config) config)
       (mapcat (fn [x]
                 (conj (map #(get-in x [% :background-image])
                            [:header :left :footer :right])
                       (get x :background-image))))
       (filterv some?)))

(defn is-image
  [content]
  (contains? content :image))

(defn foreground-images
  [contents]
  (mapcat (fn [content]
            (cond
              (is-image content) [(:image content)]
              (is-complex-component content) (foreground-images (content-children content))
              :else nil))
    contents))

(defn preload-images
  [config]
  (->> (:pages config)
       (mapcat (juxt :header :footer :left :right identity))
       (filter some?)
       (mapcat :contents)
       (foreground-images)
       (filter some?)
       (concat (background-images config))
       (distinct)
       (map #(set! (.-src (js/Image.)) (str "./images/" %)))
       (doall)))

(defn adapt-parts
  [old-parts new-parts]
  (reduce-indexed (fn [acc index new-part]
                    (let [old-part (get old-parts index)]
                      (if (or (nil? new-part)
                              (nil? old-part)
                              (>= index (count old-parts)))
                        acc
                        (update acc index assoc
                                :current (min (:current old-part) (dec (:length new-part)))
                                :children (adapt-parts (:children old-part)
                                                       (:children new-part))))))
                  (to-first-part new-parts)
                  new-parts))

(defn init-parts
  [page parts]
  (if (or (nil? parts))
    (to-first-part (build-parts (:contents page)))
    (adapt-parts parts (build-parts (:contents page)))))

(defn build-explain
  [explained]
  (let [problems (:cljs.spec.alpha/problems explained)]
    (str "Problems in config at : "
         (string/join ", " (map #(str (:in %)) (take 4 problems)))
         (when (> (count problems) 4) (str " (and " (- (count problems) 4) " more...)")))))

(defn valid-config [config]
  (when (not (s/valid? ::config config))
    (throw #js {:message (build-explain (s/explain-data ::config config))})))

(defn init-config [config]
  (valid-config config)
  (swap! app-state
    (fn [state]
      (let [{:keys [current parts]} state
            pages (:pages config)
            index (min current (dec (count pages)))
            parts (init-parts (nth pages index) parts)]
        (preload-images config)
        (-> state
            (dissoc :error)
            (assoc :config config
                   :phase :loaded
                   :current index
                   :parts parts)))))
  (set! (. js/document -title) (or-config config [:title])))

(defn fetch-error [r]
  (js/Error. (str "Unable to load config.edn (" (.-status r) " " (.-statusText r) ").")))

(defn config-error [err]
  (set! (. js/document -title) "Allalin - Error")
  (swap! app-state (fn [state]
                     (assoc state :phase :error
                                  :error err))))

(defn load-conf []
  (when (not (get-in @app-state [:config :disable-reload]))
    (set! (. js/document -title) "Allalin - Loading...")
    (swap! app-state assoc :phase :loading)
    (-> (js/fetch "./config.edn")
        (.then (fn [r]
                 (if (.-ok r)
                   r
                   (throw (fetch-error r)))))
        (.then (fn [r] (.text r)))
        (.then (fn [text]
                 (let [config (read-string text)]
                   (init-config config))))
        (.catch (fn [err] (config-error err))))))

(defn go-start []
  (swap! app-state (fn [state]
                     (let [pages (or-config (:config state) [:pages])]
                       (merge state (nth-page-parts 0 pages))))))

(defn go-page [index]
  (swap! app-state (fn [state]
                     (let [pages (or-config (:config state) [:pages])]
                       (merge state (nth-page-parts index pages))))))

(defn go-previous []
  (swap! app-state (fn [state]
                     (let [pages (or-config (:config state) [:pages])
                           {:keys [current parts]} state]
                       (merge state (dec-page-parts pages current parts))))))

(defn go-next []
  (swap! app-state (fn [state]
                     (let [pages (or-config (:config state) [:pages])
                           {:keys [current parts]} state]
                       (merge state (inc-page-parts pages current parts))))))

(defn go-end []
  (swap! app-state (fn [state]
                     (let [pages (or-config (:config state) [:pages])]
                       (merge state (last-page-parts pages))))))

(defn key-handler
  [key-event]
  (condp contains? (.-key key-event)
    #{"r" "R"} (load-conf)
    #{"f" "F"} (go-start)
    #{"Backspace" "PageUp" "ArrowLeft" "ArrowUp" "p" "P"} (go-previous)
    #{"Enter" " " "PageDown" "ArrowRight" "ArrowDown" "n" "N"} (go-next)
    #{"End"} (go-end)
    ())) ; default : no-op

; simple components

(rum/defc title < rum/static
  [content config]
  (let [content (or-default content config :title)
        tag (keyword (str "h" (:level content)))
        style (dissoc content :title :level)]
    [tag {:style style}
     (:title content)]))

(rum/defc image < rum/static
  [content config]
  (let [content (or-default content config :image)
        src (str "./images/" (:image content))
        style (dissoc content :image)]
    [:img {:src src
           :style style}]))

(defn transform-to-css
  [transform]
  (->> (if (keyword? transform) [transform] transform)
    (map (fn [key]
           (case key
             :italic [:font-style "italic"]
             :bold [:font-weight "bold"]
             :underlined [:text-decoration "underline"]
             :line-through [:text-decoration "line-through"]
             :uppercase [:text-transform "uppercase"])))
    (into {})))

(defn text-content
  [index content]
  (let [{:keys [string link transform]} content
        tag (if (some? link) :a :span)
        href (when (string? link) link)
        on-click (when (number? link) (partial go-page link))
        style (merge (dissoc content :string :link :transform)
                     (transform-to-css transform))
        attrs (cond-> {:key index}
                (some? href) (assoc :href href
                                    :target "_blank"
                                    :rel "noreferrer noopener")
                (some? style) (assoc :style style)
                (some? on-click) (assoc :on-click on-click))]
    [tag attrs string]))

(defn text-contents
  [contents]
  (->> (if (or (string? contents) (map? contents)) [contents] contents)
       (map-indexed (fn [index content]
                      (if (not (map? content))
                        content
                        (text-content index content))))))

(rum/defc text < rum/static
  [content config]
  (let [content (or-default content config :text)
        style (dissoc content :text)]
    [:div.text {:style style}
      (text-contents (:text content))]))

(rum/defc code < rum/static
  [content config]
  (let [content (or-default content config :code)
        style (dissoc content :code)]
    [:pre {:style style}
     [:code
      (text-contents (:code content))]]))

(rum/defc liste < rum/static
  [content config part]
  (let [{:keys [list contents fragmented] :as content} (or-default content config :list)
        contents (if (true? fragmented)
                   (take (inc (:current part)) contents)
                   contents)
        tag (if (= :bullet list) :ul :ol)
        style (dissoc content :list :contents :fragmented)]
    [tag {:style style}
     (map-indexed (fn [index content]
                    [:li {:key index} (text-contents content)])
       contents)]))

(rum/defc page-number
  [content config]
  (let [content (or-default content config :page-number)
        total? (= (:page-number content) :total)
        {:keys [total-pages page]} config
        style (dissoc content :page-number)]
    [:div.page-number {:style style}
     (str page (when total? (str "/" total-pages)))]))

; composed components

(declare fragments steps section)

(defn dispatch [content config part]
  (case (component-type content)
    :title (title content config)
    :image (image content config)
    :text (text content config)
    :code (code content config)
    :list (liste content config part)
    :page-number (page-number content config)
    :section (section content config part)
    :fragments (fragments content config part)
    :steps (steps content config part)))

(defn components
  [contents config parts]
  (map-indexed
    (fn [index content]
      (rum/with-key
        (dispatch content config (nth parts index))
        (str index)))
    contents))

(rum/defc section < rum/static
  [content config part]
  (let [content (or-default content config :section)
        contents (:section content)
        parts (:children part)
        style (dissoc content :section)]
    [:div.outer-section {:style style}
     [:div.inner-section
      (components contents config parts)]]))

(rum/defc fragments < rum/static
  [content config part]
  (let [index (inc (:current part))
        contents (take index (:fragments content))
        parts (take index (:children part))]
    (components contents config parts)))

(rum/defc steps < rum/static
  [content config part]
  (let [index (:current part)
        step (nth (:steps content) index)
        part (nth (:children part) index)]
    (dispatch step config [part])))

; structure components

(defn calc-vh [size ratio]
  (str "calc(" size "vh / " ratio ")"))

(defn calc-vw [size ratio]
  (str "calc(" size "vw * " ratio ")"))

(rum/defc main < rum/static
  [page config parts]
  (let [width (- 100
                 (or-config page config [:left :width])
                 (or-config page config [:right :width]))
        ratio (or-config config [:screen-ratio])
        contents (:contents page)]
    [:main {:style {:flex-basis (calc-vh width ratio)}}
     (components contents config parts)]))

(rum/defc runner < rum/static
  [key-tag page config]
  (let [str-tag (name key-tag)
        height (or-config page config [key-tag :height])
        ratio (or-config config [:screen-ratio])
        base-height (calc-vw height ratio)
        base-width (calc-vh 100 ratio)
        contents (when (> height 0) (or-config page config [key-tag :contents]))
        style (when (> height 0)
                {:background-color (or-config page config [key-tag :background-color])
                 :background-image (or-config page config [key-tag :background-image])
                 :color (or-config page config [key-tag :color])})]
    [:div.bg-ease {:class (str str-tag "-wrapper")
                   :style (merge {:flex-basis base-height} style)}
     [key-tag (components contents config nil)]
     [:style
      (str str-tag " {height: " base-height "; }
@media (min-width: " (/ 100 ratio)  "vh) { " str-tag " {height: 100%; width: " base-width "; }}")]]))

(rum/defc aside < rum/static
  [key-position page config]
  (let [str-position (name key-position)
        width (or-config page config [key-position :width])
        ratio (or-config config [:screen-ratio])
        base-width (calc-vh width ratio)
        contents (when (> width 0) (or-config page config [key-position :contents]))
        style (when (> width 0)
                {:background-color (or-config page config [key-position :background-color])
                 :background-image (or-config page config [key-position :background-image])
                 :color (or-config page config [key-position :color])})]
    [:div.aside-wrapper.bg-ease {:style (merge {:flex-basis base-width} style)}
     [:aside {:class str-position} (components contents config nil)]
     [:style
      (str "@media (min-width: " (/ 100 ratio)  "vh) { aside." str-position " {width: " base-width "; }}")]]))

(rum/defc middle < rum/static
  [page config parts]
  (let [height (- 100
                  (or-config page config [:header :height])
                  (or-config page config [:footer :height]))
        ratio (or-config config [:screen-ratio])]
    [:div.middle {:style {:flex-basis (calc-vw height ratio)}}
     (aside :left page config)
     (main page config parts)
     (aside :right page config)]))

; phase components

(rum/defc loading < rum/static
  []
  [:div.abnormal.fill
   [:div.loading
    [:h1 "Loading..."]]])

(rum/defc err < rum/static
  [error]
  (js/console.log error)
  [:div.abnormal.fill
   [:div.error
    [:h1 "Error :"]
    [:p (.-message error)]]])

(rum/defc loaded < rum/static
  [page config parts]
  [:div.root.fill
   (runner :header page config)
   (middle page config parts)
   (runner :footer page config)])

; app init

(defonce _ (load-conf))

(def key-listener-mixin
  {:did-mount (fn [state]
                (.addEventListener js/document
                  "keydown"
                  key-handler
                  false)
                (assoc state ::key-handler key-handler))
   :will-unmount (fn [state]
                   (.removeEventListener js/document
                     "keydown"
                     (::key-handler state)
                     false))})

(rum/defc app < rum/reactive key-listener-mixin
  []
  (let [{:keys [phase config current error parts]} (rum/react app-state)
        pages (or-config config [:pages])
        config (assoc config :page (inc current) :total-pages (count pages))
        page (if (= :loaded phase)
               (nth pages current)
               {})
        ratio (or-config config [:screen-ratio])
        base-width (calc-vh 100 ratio)
        bg-color (or-config page config [:background-color])
        color (or-config page config [:color])
        bg-image (or-config page config [:background-image])
        bg-position (or-config page config [:background-position])
        style {:style (cond-> {}
                        (some? bg-color) (assoc :background-color bg-color)
                        (some? color) (assoc :color color)
                        (some? bg-image) (assoc :background-image (str "url(./images/" bg-image ")")
                                                :background-size "cover"
                                                :background-position bg-position))}]
    [:div.fill.bg-ease style
     (case phase
       :loading (loading)
       :error (err error)
       :loaded (loaded page config parts))
     [:style
      (str "@media (min-width: " (/ 100 ratio)  "vh) { #app {font-size: " base-width "; }}")]]))

(rum/mount (app) (. js/document (getElementById "app")))

(defn on-js-reload [])
;; (swap! app-state update-in [:__figwheel_counter] inc)