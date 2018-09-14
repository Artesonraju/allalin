(ns allalin.mode.basic
  (:require
    [rum.core :as rum]
    [clojure.string :as string]
    [allalin.utils :refer [reduce-indexed]]
    [allalin.state :as state]
    [allalin.component :as comp]))

(def draw-height 1000)
(def swipe-sensitive 40)

(def actions {:inner-link state/go-to-page!}) ; arg: number

(defn or-config [page config keys]
  (or (get-in page keys) (get-in config keys)))

(rum/defc main < rum/static
  [page config pagings props scale]
  (let [components (:children page)
        left-width (or-config page config [:left :width])
        right-width (or-config page config [:right :width])
        header-height (or-config page config [:header :height])
        footer-height (or-config page config [:footer :height])
        ratio (:screen-ratio config)
        width (- 100 left-width right-width)
        height (- 100 header-height footer-height)]
    [:main {:style {:min-width (str width "vw")
                    :max-width (str (/ width ratio) "vh")
                    :min-height (str height "vh")
                    :max-height (str (* height ratio) "vw")}}
     [:div.trans {:style {:width (* (/ width 100) (/ draw-height ratio))
                          :height (* (/ height 100) draw-height)
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components pagings props)]]))

(rum/defc header < rum/static
  [page config props header-left header-right scale]
  (let [has-left (= :header header-left)
        has-right (= :header header-right)
        ratio (:screen-ratio config)
        height (or-config page config [:header :height])
        left-width (or-config page config [:left :width])
        right-width (or-config page config [:right :width])
        width (- 100 (if has-left 0 left-width) (if has-right 0 right-width))
        components (when (> height 0) (or-config page config [:header :children]))]
    [:header {:style {:grid-column-start (if has-left 2 3)
                      :grid-column-end (if has-right 5 4)}}
     [:div.trans {:style {:width (* (/ width 100) (/ draw-height ratio))
                          :height (* (/ height 100) draw-height)
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components nil props)]]))

(rum/defc footer < rum/static
  [page config props footer-left footer-right scale]
  (let [has-left (= :footer footer-left)
        has-right (= :footer footer-right)
        ratio (:screen-ratio config)
        height (or-config page config [:footer :height])
        left-width (or-config page config [:left :width])
        right-width (or-config page config [:right :width])
        width (- 100 (if has-left 0 left-width) (if has-right 0 right-width))
        components (when (> height 0) (or-config page config [:footer :children]))]
    [:footer {:style {:grid-column-start (if has-left 2 3)
                      :grid-column-end (if has-right 5 4)
                      :min-height (str height "vh")
                      :max-height (str (* height ratio) "vw")}}
     [:div.trans {:style {:width (* (/ width 100) (/ draw-height ratio))
                          :height (* (/ height 100) draw-height)
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components nil props)]]))

(rum/defc left < rum/static
  [page config props header-left footer-left scale]
  (let [has-header (= :left header-left)
        has-footer (= :left footer-left)
        ratio (:screen-ratio config)
        width (or-config page config [:left :width])
        header-height (or-config page config [:header :height])
        footer-height (or-config page config [:footer :height])
        height (- 100 (if has-header 0 header-height) (if has-footer 0 footer-height))
        components (when (> width 0) (or-config page config [:left :children]))]
    [:aside.left {:style {:grid-row-start (if has-header 2 3)
                          :grid-row-end (if has-footer 5 4)
                          :min-width (str width "vw")
                          :max-width (str (/ width ratio) "vh")}}
     [:div.trans {:style {:width (* (/ width 100) (/ draw-height ratio))
                          :height (* (/ height 100) draw-height)
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components nil props)]]))

(rum/defc right < rum/static
  [page config props header-right footer-right scale]
  (let [has-header (= :right header-right)
        has-footer (= :right footer-right)
        ratio (:screen-ratio config)
        width (or-config page config [:right :width])
        header-height (or-config page config [:header :height])
        footer-height (or-config page config [:footer :height])
        height (- 100 (if has-header 0 header-height) (if has-footer 0 footer-height))
        components (when (> width 0) (or-config page config [:right :children]))]
    [:aside.right {:style {:grid-row-start (if has-header 2 3)
                           :grid-row-end (if has-footer 5 4)}}
     [:div.trans {:style {:width (* (/ width 100) (/ draw-height ratio))
                          :height (* (/ height 100) draw-height)
                          :transform (str "scale(" scale ")")}}
      (comp/render-children components nil props)]]))

(rum/defc runner-wrapper < rum/static
  [key-tag page config]
  (let [height (or-config page config [key-tag :height])
        style (when (> height 0)
                {:background-color (or-config page config [key-tag :background-color])
                 :background-image (or-config page config [key-tag :background-image])
                 :color (or-config page config [key-tag :color])})]
    [:div.bg-ease {:class (str (name key-tag) "-wrapper")
                   :style style}]))

(rum/defc aside-wrapper < rum/static
  [key-position page config]
  (let [width (or-config page config [key-position :width])
        style (when (> width 0)
                {:background-color (or-config page config [key-position :background-color])
                 :background-image (or-config page config [key-position :background-image])
                 :color (or-config page config [key-position :color])})]
    [:div.bg-ease {:class (str (name key-position) "-wrapper")
                   :style style}]))

(rum/defc progress-bar
  [config position scale]
  (let [{:keys [side width color]} (:progress-bar config)]
    (when (and (some? side) (> width 0))
      (let [{:keys [current total]} (:counts position)
            vertical? (contains? #{:left :right} side)
            length (/ current (dec total))
            ratio (:screen-ratio config)
            style (merge {:background-color color
                          side 0}
                         (if vertical? {:width (* (/ width 100) (/ draw-height ratio))
                                        :height (str (* 100 length) "%")
                                        :transform (str "scale(" scale ", 1)")
                                        :transform-origin (str "top " (name side));
                                        :top 0}
                                       {:width (str (* 100 length) "%")
                                        :height (* (/ width 100) draw-height)
                                        :transform (str "scale(1, " scale ")")
                                        :transform-origin (str (name side) " left");
                                        :left 0}))]
        [:div.progress {:style style}]))))

; phase components

(defn corner-area
  [runner aside page config]
  (let [height (or-config page config [runner :height])
        width (or-config page config [aside :width])
        default (or-config page config [(keyword (str (name runner) "-" (name aside)))])]
    (if (or (and (= runner default) (or (> height 0) (= width 0)))
            (and (> height 0) (= width 0)))
      runner
      aside)))

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


(def size-listener-mixin
  { :did-mount    (fn [state]
                    (let [comp (:rum/react-component state)
                          resize-handler #(rum/request-render comp)]
                      (.addEventListener js/window
                                         "resize"
                                         resize-handler
                                         false)
                      (assoc state ::resize-handler resize-handler)))
   :will-unmount (fn [state]
                   (.removeEventListener js/document
                                         "resize"
                                         (::resize-handler state)
                                         false)
                   (dissoc state ::resize-handler))})

(defn basic-style [page config]
  (let [bg-color (or-config page config [:background-color])
        color (or-config page config [:color])
        bg-image (or-config page config [:background-image])
        bg-position (or-config page config [:background-position])]
    (cond-> {}
      (some? bg-color) (assoc :background-color bg-color)
      (some? color) (assoc :color color)
      (some? bg-image) (assoc :background-image (str "url(./images/" bg-image ")")
                              :background-size "cover"
                              :background-position bg-position))))

(def action-keys {state/go-start! ["Home"]
                  state/go-next! ["Enter" " " "PageDown" "ArrowRight" "ArrowDown" "n" "N"]
                  state/go-previous! ["Backspace" "PageUp" "ArrowLeft" "ArrowUp" "p" "P"]
                  state/go-end! ["End"]})

(rum/defc basic < size-listener-mixin touch-listener-mixin
  [config position]
  (let [{:keys [paging counts]} position
        {:keys [pages default]} config
        current (:current counts)
        [page pagings] ((juxt (partial nth pages) (partial nth paging)) current)
        props {:default default :actions actions :counts counts}
        style (basic-style page config)
        header-left (corner-area :header :left page config)
        header-right (corner-area :header :right page config)
        footer-left (corner-area :footer :left page config)
        footer-right (corner-area :footer :right page config)                window-height (.-innerHeight js/window)
        window-width (.-innerWidth js/window)
        screen-ratio (:screen-ratio config)
        window-ratio (/ window-height window-width)
        higher? (> window-ratio screen-ratio)
        scale (if (> screen-ratio window-ratio)
                (/ window-height draw-height)
                (/ window-width (/ draw-height screen-ratio)))

        header-height (or-config page config [:header :height])
        left-width (or-config page config [:left :width])
        footer-height (or-config page config [:footer :height])
        right-width (or-config page config [:right :width])
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
    [:div.root.bg-ease.fill {:style (merge style grid)}
     (runner-wrapper :header page config)
     (aside-wrapper :left page config)
     (aside-wrapper :right page config)
     (runner-wrapper :footer page config)
     (main page config pagings props scale)
     (header page config props header-left header-right scale)
     (left page config props header-left footer-left scale)
     (footer page config props footer-left footer-right scale)
     (right page config props header-right footer-right scale)
     (progress-bar config position scale)]))