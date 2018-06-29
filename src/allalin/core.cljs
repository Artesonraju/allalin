(ns allalin.core
  (:require [rum.core :as rum]
            [cljs.reader :refer [read-string]]))

(enable-console-print!)

; default values

(def default-config {:screen-ratio 0.625
                     :background-color "#fff"
                     :background-position "center"
                     :title "Allalin - Presentation framework"
                     :default {:title {:level 2}}
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
                             :content []}
                     :pages [{:title "Allalin - Presentation Framework"}]})

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

(defn or-config
  ([config keys] (get-in config keys (get-in default-config keys)))
  ([page config keys] (get-in page keys (or-config config keys))))

(defn or-default
  [content config element key]
  (get content key (or-config config [:default element key])))

(defn or-defaults
  [content config element keys]
  (reduce (fn [acc key]
            (let [val (or-default content config element key)]
              (if (some? val)
                (assoc acc key val)
                acc)))
          {}
          keys))

; state

(defonce app-state (atom {:phase :loading
                          :current 0}))

; simple components

(def simple-components #{:title :image :rum :code :page-number})

(rum/defc title < rum/static
  [content config]
  (let [level (or (:level content) (or-config config [:default :title :level]))
        tag (keyword (str "h" level))]
    [tag {:style (or-defaults content config :title [:color :font-size :text-align :top :left :width])}
     (:title content)]))

(rum/defc image < rum/static
  [content config]
  (let [src (str "./images/" (:image content))]
    [:img {:src src
           :style (or-defaults content config :image [:top :left :width :height])}]))

(rum/defc html < rum/static
  [content config]
  (let [rum-content (->> (:rum content)
                         (map-indexed (fn [index content]
                                        (cond
                                          (not (vector? content)) content
                                          (map? (second content)) (assoc-in content [1 :key] index)
                                          :else (into [(first content) {:key index}] (rest content))))))]
    [:div.rum {:style (or-defaults content config :rum [:color :font-size :text-align :top :left :width])}
      rum-content]))

(rum/defc code < rum/static
  [content config]
  (let [rum-content (->> (:code content)
                         (map-indexed (fn [index content]
                                        (cond
                                          (not (vector? content)) content
                                          (map? (second content)) (assoc-in content [2 :key] index)
                                          :else (into [(first content) {:key index}] (rest content))))))]
    [:pre {:style (or-defaults content config :rum [:color :font-size :text-align :top :left :width])}
     [:code
       rum-content]]))

(rum/defc page-number
  [content config]
  (let [total? (= (:page-number content) :total)
        {:keys [total-pages page]} config]
    [:div.page-number {:style (or-defaults content config :page-number [:color :font-size :top :left :width])}
     (str page (when total? (str "/" total-pages)))]))

; composed components

(def complex-components #{:fragments :steps :section})

(def changing-components #{:fragments :steps})

(declare fragments steps section)

(defn dispatch [content config part]
  (condp #(contains? %2 %1) content
    :title (title content config)
    :image (image content config)
    :rum (html content config)
    :code (code content config)
    :page-number (page-number content config)
    :fragments (fragments content config part)
    :steps (steps content config part)
    :section (section content config part)))

(defn components
  [contents config parts]
  (map-indexed
    (fn [index content]
      (rum/with-key
        (dispatch content config (nth parts index))
        (str index)))
    contents))

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

(rum/defc section < rum/static
  [content config part]
  (let [contents (:section content)
        parts (:children part)]
    [:div.section {:style (or-defaults content config :rum [:color :background-color :border :top :left :width :height])}
     (components contents config parts)]))

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
        contents (or-config page config [key-tag :contents])
        style {:background-color (or-config page config [key-tag :background-color])
               :background-image (or-config page config [key-tag :background-image])
               :color (or-config page config [key-tag :color])
               :flex-basis base-height}]
    [:div.bg-ease {:class (str str-tag "-wrapper")
                   :style style}
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
        contents (or-config page config [key-position :contents])
        style {:background-color (or-config page config [key-position :background-color])
               :background-image (or-config page config [key-position :background-image])
               :color (or-config page config [key-position :color])
               :flex-basis base-width}]
    [:div.aside-wrapper.bg-ease {:style style}
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

; page and parts management

(defn is-complex-component
  [content]
  (some #(contains? content %) complex-components))

(defn is-changing-component
  [content]
  (some #(contains? content %) changing-components))

(defn content-children
  [content]
  (or (:fragments content)
      (:steps content)
      (:section content)))

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

(defn first-page-parts [pages]
  (let [page (first pages)]
    {:current 0
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

(defn add-default-pages [config]
  (cond-> config
    (= 0 (count (:pages config))) (assoc :pages (:pages default-config))))

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
  (->> contents
    (mapcat (fn [content]
              (cond
                (is-image content) (vec (:image content))
                (is-complex-component content) (foreground-images (content-children content))
                :else nil)))
    (filterv some?)))

(defn preload-images
  [config]
  (let [inner-images (->> (:pages config)
                          (mapcat #(foreground-images (:contents %))))]
    (->> (concat inner-images (background-images config))
         (map #(set! (.-src (js/Image.)) (str "./images/" %)))
         (doall))))

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

(defn init-config [config]
  (set! (. js/document -title) (or-config config [:title]))
  (swap! app-state
    (fn [state]
      (let [{:keys [current parts]} state
            config (add-default-pages config)
            pages (:pages config)
            index (min current (dec (count pages)))
            parts (init-parts (nth pages index) parts)]
        (preload-images config)
        (-> state
            (dissoc :error)
            (assoc :config config
                   :phase :loaded
                   :current index
                   :parts parts))))))

(defn fetch-error [r]
  (js/Error. (str "Unable to load config.edn (" (.-status r) " " (.-statusText r) ").")))

(defn config-error [err]
  (set! (. js/document -title) "Allalin - Error")
  (swap! app-state (fn [state]
                     (assoc state :phase :error
                                  :error err))))

(defn load-conf []
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
      (.catch (fn [err] (config-error err)))))

(defn go-start []
  (swap! app-state (fn [state]
                     (let [pages (or-config (:config state) [:pages])]
                       (merge state (first-page-parts pages))))))

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

; app init

(defonce _ (load-conf))

(def key-listener-mixin
  {:did-mount (fn [state]
                (.addEventListener js/document
                  "keypress"
                  key-handler
                  false)
                (assoc state ::key-handler key-handler))
   :will-unmount (fn [state]
                   (.removeEventListener js/document
                     "keypress"
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
        bg-image (or-config page config [:background-image])
        bg-position (or-config page config [:background-position])
        style {:style (cond-> {}
                        (some? bg-color) (assoc :background-color bg-color)
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