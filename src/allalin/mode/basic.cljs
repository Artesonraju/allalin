(ns allalin.mode.basic
  (:require
    [rum.core :as rum]
    [clojure.string :as string]
    [allalin.state :as state]
    [allalin.component :as comp]
    [allalin.slide :as s]))

(def swipe-sensitive 40)

(def actions {:inner-link (comp #(state/go-to-page! (if (zero? %) % (dec %))))})

(rum/defc main < rum/static
  [page config part props scale]
  (let [components (:children page)
        point {:part part :stock (-> props :counts :stock)}
        left-width (s/or-config page config [:left :width])
        right-width (s/or-config page config [:right :width])
        header-height (s/or-config page config [:header :height])
        footer-height (s/or-config page config [:footer :height])
        ratio (:screen-ratio config)
        width (- 100 left-width right-width)
        height (- 100 header-height footer-height)]
    [:main {:style {:min-width (str width "vw")
                    :max-width (str (/ width ratio) "vh")
                    :min-height (str height "vh")
                    :max-height (str (* height ratio) "vw")}}
     [:div.trans {:style {:width (* (/ width 100) (/ s/draw-height ratio))
                          :height (* (/ height 100) s/draw-height)
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components point props)]]))

(rum/defc header < rum/static
  [page config props header-left header-right scale]
  (let [has-left (= :header header-left)
        has-right (= :header header-right)
        ratio (:screen-ratio config)
        height (s/or-config page config [:header :height])
        left-width (s/or-config page config [:left :width])
        right-width (s/or-config page config [:right :width])
        width (- 100 (if has-left 0 left-width) (if has-right 0 right-width))
        components (when (> height 0) (s/or-config page config [:header :children]))]
    [:header {:style {:grid-column-start (if has-left 2 3)
                      :grid-column-end (if has-right 5 4)}}
     [:div.trans {:style {:width (* (/ width 100) (/ s/draw-height ratio))
                          :height (* (/ height 100) s/draw-height)
                          :color (s/or-config page config [:header :color])
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components nil props)]]))

(rum/defc footer < rum/static
  [page config props footer-left footer-right scale]
  (let [has-left (= :footer footer-left)
        has-right (= :footer footer-right)
        ratio (:screen-ratio config)
        height (s/or-config page config [:footer :height])
        left-width (s/or-config page config [:left :width])
        right-width (s/or-config page config [:right :width])
        width (- 100 (if has-left 0 left-width) (if has-right 0 right-width))
        components (when (> height 0) (s/or-config page config [:footer :children]))]
    [:footer {:style {:grid-column-start (if has-left 2 3)
                      :grid-column-end (if has-right 5 4)
                      :min-height (str height "vh")
                      :max-height (str (* height ratio) "vw")}}
     [:div.trans {:style {:width (* (/ width 100) (/ s/draw-height ratio))
                          :height (* (/ height 100) s/draw-height)
                          :color (s/or-config page config [:footer :color])
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components nil props)]]))

(rum/defc left < rum/static
  [page config props header-left footer-left scale]
  (let [has-header (= :left header-left)
        has-footer (= :left footer-left)
        ratio (:screen-ratio config)
        width (s/or-config page config [:left :width])
        header-height (s/or-config page config [:header :height])
        footer-height (s/or-config page config [:footer :height])
        height (- 100 (if has-header 0 header-height) (if has-footer 0 footer-height))
        components (when (> width 0) (s/or-config page config [:left :children]))]
    [:aside.left {:style {:grid-row-start (if has-header 2 3)
                          :grid-row-end (if has-footer 5 4)
                          :min-width (str width "vw")
                          :max-width (str (/ width ratio) "vh")}}
     [:div.trans {:style {:width (* (/ width 100) (/ s/draw-height ratio))
                          :height (* (/ height 100) s/draw-height)
                          :color (s/or-config page config [:left :color])
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components nil props)]]))

(rum/defc right < rum/static
  [page config props header-right footer-right scale]
  (let [has-header (= :right header-right)
        has-footer (= :right footer-right)
        ratio (:screen-ratio config)
        width (s/or-config page config [:right :width])
        header-height (s/or-config page config [:header :height])
        footer-height (s/or-config page config [:footer :height])
        height (- 100 (if has-header 0 header-height) (if has-footer 0 footer-height))
        components (when (> width 0) (s/or-config page config [:right :children]))]
    [:aside.right {:style {:grid-row-start (if has-header 2 3)
                           :grid-row-end (if has-footer 5 4)}}
     [:div.trans {:style {:width (* (/ width 100) (/ s/draw-height ratio))
                          :height (* (/ height 100) s/draw-height)
                          :color (s/or-config page config [:right :color])
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components nil props)]]))

(rum/defc runner-wrapper < rum/static
  [key-tag page config]
  (let [height (s/or-config page config [key-tag :height])
        style (when (> height 0)
                {:background-color (s/or-config page config [key-tag :background-color])
                 :background-image (s/or-config page config [key-tag :background-image])})]
    [:div.bg-ease {:class (str (name key-tag) "-wrapper")
                   :style style}]))

(rum/defc aside-wrapper < rum/static
  [key-position page config]
  (let [width (s/or-config page config [key-position :width])
        style (when (> width 0)
                {:background-color (s/or-config page config [key-position :background-color])
                 :background-image (s/or-config page config [key-position :background-image])})]
    [:div.bg-ease {:class (str (name key-position) "-wrapper")
                   :style style}]))

(def touch-position (atom {}))

(defn x-touch [event]
  (-> event
      (.-changedTouches)
      (aget 0)
      (.-screenX)))

(def touch-listener-mixin
  {:did-mount (fn [state]
                (let [start-handler #(swap! touch-position assoc :x (x-touch %))
                      end-handler (fn [e]
                                    (let [x (x-touch e)
                                          x-start (:x @touch-position)]
                                      (cond
                                        (< x (- x-start swipe-sensitive)) (state/go-next!)
                                        (> x (+ x-start swipe-sensitive)) (state/go-previous!))))]
                  (.addEventListener js/document
                                     "touchstart"
                                     start-handler
                                     false)
                  (.addEventListener js/document
                                     "touchend"
                                     end-handler
                                     false)
                  (assoc state ::touchstart-handler start-handler
                               ::touchend-handler end-handler)))
   :will-unmount (fn [state]
                   (.removeEventListener js/document
                                         "touchstart"
                                         (::touchstart-handler state)
                                         false)
                   (.removeEventListener js/document
                                         "touchend"
                                         (::touchend-handler state)
                                         false)
                   (dissoc state ::touchstart-handler ::touchend-handler))})

(rum/defc basic < s/size-listener-mixin touch-listener-mixin
  [config position]
  (let [window-height (.-innerHeight js/window)
        window-width (.-innerWidth js/window)
        {:keys [parts counts]} position
        {:keys [pages default]} config
        current (:current counts)
        [page part] ((juxt (partial nth pages) (partial nth parts)) current)
        props {:default default :actions actions :counts counts}
        header-left (s/corner-area :header :left page config)
        header-right (s/corner-area :header :right page config)
        footer-left (s/corner-area :footer :left page config)
        footer-right (s/corner-area :footer :right page config)
        screen-ratio (:screen-ratio config)
        window-ratio (/ window-height window-width)
        higher? (> window-ratio screen-ratio)
        scale (if higher?
                (/ window-width (/ s/draw-height screen-ratio))
                (/ window-height s/draw-height))
        header-height (s/or-config page config [:header :height])
        left-width (s/or-config page config [:left :width])
        footer-height (s/or-config page config [:footer :height])
        right-width (s/or-config page config [:right :width])
        main-height (- 100 header-height footer-height)
        main-width (- 100 left-width right-width)
        grid {:grid-template-rows (if higher?
                                    (str "1fr " (* header-height screen-ratio) "vw"
                                         " " (* main-height screen-ratio) "vw"
                                         " " (* footer-height screen-ratio) "vw"
                                         " 1fr")
                                    (str "1fr " header-height "vh"
                                         " " main-height "vh"
                                         " " footer-height "vh"
                                         " 1fr"))
              :grid-template-columns (if higher?
                                       (str "1fr " left-width "vw"
                                            " " main-width "vw"
                                            " " right-width "vw"
                                            " 1fr")
                                       (str "1fr " (/ left-width screen-ratio) "vh"
                                            " " (/ main-width screen-ratio) "vh"
                                            " " (/ right-width screen-ratio) "vh"
                                            " 1fr"))
              :grid-template-areas (str "'" (string/join " " (map name [header-left header-left :header header-right header-right])) "'\n"
                                        "'" (string/join " " (map name [header-left header-left :header header-right header-right])) "'\n"
                                        "'" (string/join " " (map name [:left :left :main :right :right])) "'\n"
                                        "'" (string/join " " (map name [footer-left footer-left :footer footer-right footer-right])) "'\n"
                                        "'" (string/join " " (map name [footer-left footer-left :footer footer-right footer-right])) "'")}]
    [:div.basic.root.bg-ease {:style (merge (s/basic-style page config) grid)}
     (runner-wrapper :header page config)
     (aside-wrapper :left page config)
     (aside-wrapper :right page config)
     (runner-wrapper :footer page config)
     (main page config part props scale)
     (header page config props header-left header-right scale)
     (left page config props header-left footer-left scale)
     (footer page config props footer-left footer-right scale)
     (right page config props header-right footer-right scale)
     (s/progress-bar config counts scale)]))