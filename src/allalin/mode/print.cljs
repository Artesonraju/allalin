(ns allalin.mode.print
  (:require
    [rum.core :as rum]
    [clojure.string :as string]
    [allalin.slide :as s]
    [allalin.state :as state]))

(def print-width 1500) ;px
(def margin-step 20) ;px

(def action-keys {(partial state/add-print-margin! margin-step) ["+" "="]
                  (partial state/add-print-margin! (- margin-step)) ["-"]})

(rum/defc slide < s/size-listener-mixin
  [page config part props margin]
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
      (s/main page part props)
      (s/header page config props)
      (s/left page config props)
      (s/footer page config props)
      (s/right page config props)
      (s/progress-bar config (:counts props) 1)]]))

(rum/defc print- < rum/static
  [config position print]
  (let [{:keys [counts parts]} position
        {:keys [pages default]} config
        margin (or (:margin print) 0)
        margin-fn (fn [i] (if (pos? i) margin 0))]
    [:div.print
     (map-indexed
       (fn [index page]
         (let [props {:default default :counts (assoc counts :current index)}]
           (rum/with-key
             (slide page config (nth parts index) props (margin-fn index))
             (str index))))
       pages)]))

