(ns allalin.mode.print
  (:require
    [rum.core :as rum]
    [allalin.slide :as s]
    [allalin.state :as state]))

(def print-width 1500) ;px
(def margin-step 20) ;px

(def action-keys [[(partial state/add-print-margin! margin-step)
                   {:keys ["+" "="]
                    :tip (s/tip ["+"] "increase margin between slides")}]
                  [(partial state/add-print-margin! (- margin-step))
                   {:keys ["-"]
                    :tip (s/tip ["−"] "decrease margin between slides")}]])

(rum/defc slide < s/size-listener-mixin
  [page config part props margin]
  (let [screen-ratio (:screen-ratio config)]
    [:div {:style {:min-height (* print-width screen-ratio)
                   :max-height (* print-width screen-ratio)
                   :margin-top (str margin "px")}}
     (s/slide page config part props print-width)]))

(rum/defc print- < rum/static
  [config position print]
  (let [{:keys [counts parts]} position
        {:keys [pages default]} config
        margin (or (:margin print) 0)
        margin-fn (fn [i] (if (pos? i) margin 0))]
    [:div.print
     (map-indexed
       (fn [index page]
         (let [props {:default default :counts (-> counts
                                                   (assoc :current index)
                                                   (dissoc :stock))}]
           (rum/with-key
             (slide page config (nth parts index) props (margin-fn index))
             (str index))))
       pages)]))

