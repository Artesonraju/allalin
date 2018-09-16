(ns allalin.mode.print
  (:require
    [rum.core :as rum]
    [clojure.string :as string]
    [allalin.slide :as s]
    [allalin.component :as comp]
    [allalin.state :as state]))

(def print-width 1500) ;px
(def margin-step 20)


(def action-keys {(partial state/add-print-margin! margin-step) ["+" "="]
                  (partial state/add-print-margin! (- margin-step)) ["-"]})

(rum/defc main < rum/static
  [page pagings props]
  (let [components (:children page)]
    [:main
     [:div.trans
      (comp/render-children components pagings props)]]))

(rum/defc header < rum/static
  [page config props]
  (let [height (s/or-config page config [:header :height])
        components (when (> height 0) (s/or-config page config [:header :children]))]
    [:header {:style {:grid-area (name :header)
                      :background-color (s/or-config page config [:header :background-color])
                      :background-image (s/or-config page config [:header :background-image])}}
     [:div.trans {:style {:color (s/or-config page config [:header :color])}}
      (comp/render-children components nil props)]]))

(rum/defc footer < rum/static
  [page config props]
  (let [height (s/or-config page config [:footer :height])
        components (when (> height 0) (s/or-config page config [:footer :children]))]
    [:footer {:style {:grid-area (name :footer)
                      :background-color (s/or-config page config [:footer :background-color])
                      :background-image (s/or-config page config [:footer :background-image])}}
     [:div.trans {:style {:color (s/or-config page config [:footer :color])}}
      (comp/render-children components nil props)]]))

(rum/defc left < rum/static
  [page config props]
  (let [width (s/or-config page config [:left :width])
        components (when (> width 0) (s/or-config page config [:left :children]))]
    [:aside.left {:style {:grid-area (name :left)
                          :background-color (s/or-config page config [:left :background-color])
                          :background-image (s/or-config page config [:left :background-image])}}
     [:div.trans {:style {:color (s/or-config page config [:left :color])}}
      (comp/render-children components nil props)]]))

(rum/defc right < rum/static
  [page config props]
  (let [width (s/or-config page config [:right :width])
        components (when (> width 0) (s/or-config page config [:right :children]))]
    [:aside.right {:style {:grid-area (name :right)
                           :background-color (s/or-config page config [:right :background-color])
                           :background-image (s/or-config page config [:right :background-image])}}
     [:div.trans {:style {:color (s/or-config page config [:right :color])}}
      (comp/render-children components nil props)]]))

(rum/defc slide < s/size-listener-mixin
  [page config pagings props margin]
  (let [style (s/basic-style page config)
        header-left (s/corner-area :header :left page config)
        header-right (s/corner-area :header :right page config)
        footer-left (s/corner-area :footer :left page config)
        footer-right (s/corner-area :footer :right page config)
        screen-ratio (:screen-ratio config)
        scale (/ print-width (/ s/draw-height screen-ratio))
        header-height (s/or-config page config [:header :height])
        left-width (s/or-config page config [:left :width])
        footer-height (s/or-config page config [:footer :height])
        right-width (s/or-config page config [:right :width])
        main-height (- 100 header-height footer-height)
        main-width (- 100 left-width right-width)
        grid {:width (str (/ s/draw-height screen-ratio) "px")
              :height (str s/draw-height "px")
              :transform (str "scale(" scale ")")
              :grid-template-rows (str (* (/ header-height 100) s/draw-height) "px"
                                       " " (* (/ main-height 100) s/draw-height) "px"
                                       " " (* (/ footer-height 100) s/draw-height) "px")
              :grid-template-columns (str (* (/ left-width 100) (/ s/draw-height screen-ratio)) "px"
                                          " " (* (/ main-width 100) (/ s/draw-height screen-ratio)) "px"
                                          " " (* (/ right-width 100) (/ s/draw-height screen-ratio)) "px")
              :grid-template-areas (str "'" (string/join " " (map name [header-left :header header-right])) "'\n"
                                        "'" (string/join " " (map name [:left :main :right])) "'\n"
                                        "'" (string/join " " (map name [footer-left :footer footer-right])) "'")}]
    [:div {:style {:min-height (* print-width screen-ratio)
                   :max-height (* print-width screen-ratio)
                   :margin-top (str margin "px")}}
     [:div.root.fill {:style (merge style grid)}
      (main page pagings props)
      (header page config props)
      (left page config props)
      (footer page config props)
      (right page config props)
      (s/progress-bar config (:counts props) 1)]]))

(rum/defc print- < rum/static
  [config position print]
  (let [{:keys [all-paging counts]} position
        {:keys [pages default]} config
        margin (or (:margin print) 0)
        margin-fn (fn [i] (if (pos? i) margin 0))]
    [:div.print
     (map-indexed
       (fn [index page]
         (let [paging (nth all-paging index)
               props {:default default :counts (assoc counts :current index)}]
           (rum/with-key
             (slide page config paging props (margin-fn index))
             (str index))))
       pages)]))

