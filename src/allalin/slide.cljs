(ns allalin.slide
  (:require
    [clojure.string :as string]
    [rum.core :as rum]
    [allalin.component :as comp]))

(def draw-height 1000)

(defn or-config [page config keys]
  (or (get-in page keys) (get-in config keys)))

(defn key-
  ([k] (key- k false))
  ([k modifier?]
   [:span.key {:key k
               :class [(when modifier? "modifier")
                       (when (> (count k) 1) "small")]}
    k]))

(defn combination [c]
  (if (vector? c)
    (->> (conj (map key- (rest c))
               (key- (first c) true))
         (interpose "+"))
    (key- c)))

(defn tip [combinations explanation]
  [:div.pane-tip
   (->> combinations
        (map combination)
        (interpose " / "))
   (str " " explanation)])

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

(rum/defc progress-bar
  [config counts scale]
  (let [{:keys [side width color]} (:progress-bar config)]
    (when (and (some? side) (> width 0))
      (let [{:keys [current total]} counts
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

(defn corner-area
  [runner aside page config]
  (let [height (or-config page config [runner :height])
        width (or-config page config [aside :width])
        default (or-config page config [(keyword (str (name runner) "-" (name aside)))])]
    (if (or (and (= runner default) (or (> height 0) (= width 0)))
            (and (> height 0) (= width 0)))
      runner
      aside)))

(rum/defc main < rum/static
  [page part props]
  (let [components (:children page)
        stock (or (-> props :counts :stock) (:moves part))
        point {:part part :stock stock}]
    [:main
     [:div.trans
      (comp/render-children components point props)]]))

(rum/defc header < rum/static
  [page config props]
  (let [height (or-config page config [:header :height])
        components (when (> height 0) (or-config page config [:header :children]))]
    [:header {:style {:grid-area (name :header)
                      :background-color (or-config page config [:header :background-color])
                      :background-image (or-config page config [:header :background-image])}}
     [:div.trans {:style {:color (or-config page config [:header :color])}}
      (comp/render-children components nil props)]]))

(rum/defc footer < rum/static
  [page config props]
  (let [height (or-config page config [:footer :height])
        components (when (> height 0) (or-config page config [:footer :children]))]
    [:footer {:style {:grid-area (name :footer)
                      :background-color (or-config page config [:footer :background-color])
                      :background-image (or-config page config [:footer :background-image])}}
     [:div.trans {:style {:color (or-config page config [:footer :color])}}
      (comp/render-children components nil props)]]))

(rum/defc left < rum/static
  [page config props]
  (let [width (or-config page config [:left :width])
        components (when (> width 0) (or-config page config [:left :children]))]
    [:aside.left {:style {:grid-area (name :left)
                          :background-color (or-config page config [:left :background-color])
                          :background-image (or-config page config [:left :background-image])}}
     [:div.trans {:style {:color (or-config page config [:left :color])}}
      (comp/render-children components nil props)]]))

(rum/defc right < rum/static
  [page config props]
  (let [width (or-config page config [:right :width])
        components (when (> width 0) (or-config page config [:right :children]))]
    [:aside.right {:style {:grid-area (name :right)
                           :background-color (or-config page config [:right :background-color])
                           :background-image (or-config page config [:right :background-image])}}
     [:div.trans {:style {:color (or-config page config [:right :color])}}
      (comp/render-children components nil props)]]))

(rum/defc slide
  [page config part props print-width]
  (let [header-left (corner-area :header :left page config)
        header-right (corner-area :header :right page config)
        footer-left (corner-area :footer :left page config)
        footer-right (corner-area :footer :right page config)
        screen-ratio (:screen-ratio config)
        scale (/ print-width (/ draw-height screen-ratio))
        header-height (or-config page config [:header :height])
        left-width (or-config page config [:left :width])
        footer-height (or-config page config [:footer :height])
        right-width (or-config page config [:right :width])
        main-height (- 100 header-height footer-height)
        main-width (- 100 left-width right-width)
        grid {:width (str (/ draw-height screen-ratio) "px")
              :height (str draw-height "px")
              :transform (str "scale(" scale ")")
              :grid-template-rows (str (* (/ header-height 100) draw-height) "px"
                                       " " (* (/ main-height 100) draw-height) "px"
                                       " " (* (/ footer-height 100) draw-height) "px")
              :grid-template-columns (str (* (/ left-width 100) (/ draw-height screen-ratio)) "px"
                                          " " (* (/ main-width 100) (/ draw-height screen-ratio)) "px"
                                          " " (* (/ right-width 100) (/ draw-height screen-ratio)) "px")
              :grid-template-areas (str "'" (string/join " " (map name [header-left :header header-right])) "'\n"
                                        "'" (string/join " " (map name [:left :main :right])) "'\n"
                                        "'" (string/join " " (map name [footer-left :footer footer-right])) "'")}]

    [:div.root.fill.slide {:style (merge (basic-style page config) grid)}
     (main page part props)
     (header page config props)
     (left page config props)
     (footer page config props)
     (right page config props)
     (progress-bar config (:counts props) 1)]))