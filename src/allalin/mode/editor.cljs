(ns allalin.mode.editor
  (:require
    [rum.core :as rum]
    [clojure.string :as string]
    [allalin.slide :as s]
    [allalin.state :as state]))

(def action-keys [[state/previous-change!
                   {:keys [[:ctrl "z"][:ctrl "Z"]]
                    :tip (s/tip [["Ctrl" "Z"]] "indo")}]
                  [state/next-change!
                   {:keys [[:ctrl "y"] [:ctrl "Y"]]
                    :tip (s/tip [["Ctrl" "Y"]] "redo")}]])

(rum/defc preview < rum/static
  [config position width]
  (let [screen-ratio (:screen-ratio config)
        {:keys [parts counts]} position
        {:keys [pages default]} config
        current (:current counts)
        [page part] ((juxt (partial nth pages) (partial nth parts)) current)
        props {:default default :counts counts}]
    [:div.preview
     [:div {:style {:height (* width screen-ratio)
                    :width width}}
      (s/slide page config part props width)]]))


(rum/defc thumbnail < s/size-listener-mixin
  [page config parts props current index thumbnail-width]
  (let [part (nth parts index)
        screen-ratio (:screen-ratio config)
        current? (= current index)
        prevent (fn [f event] (do (f event)
                                  (.stopPropagation event)))]
    [:div.thumbnail {:class (when current? "current")
                     :on-click (partial state/go-to-page! index)}
     [:button.before {:on-click (partial prevent (partial state/insert-page-at! index))
                      :title "Add a slide"}
      "+"]
     [:div.thumbnail-content {:class (when (= current index) "current")
                              :style {:height (* thumbnail-width screen-ratio)
                                      :width thumbnail-width}}
      [:div.noaction
       (s/slide page config part props thumbnail-width)]
      [:div.thumbnail-count
       (str (inc index))]]
     [:button.clone {:on-click (partial prevent (partial state/clone-page-at! index))
                     :title "Clone this slide"}
      "="]
     (when (> (count parts) 1)
       [:button.delete {:on-click (partial prevent (partial state/delete-page-at! index))
                        :title "Delete this slide"}
        "x"])
     [:button.after {:on-click (partial prevent (partial state/insert-page-at! (inc index)))
                     :title "Add a slide"}
      "+"]]))

(rum/defc thumbnails
  [config position thumbnail-width]
  (let [{:keys [counts parts]} position
        {:keys [current total]} counts
        {:keys [pages default]} config]
    [:div.thumbnails
     [:div {:style {:height (+ (* total thumbnail-width) 20)}}
      (map-indexed
        (fn [index page]
          (let [props {:default default :counts (-> counts
                                                    (assoc :current index)
                                                    (dissoc :stock))}]
            (rum/with-key
              (thumbnail page config parts props
                         current index thumbnail-width)
              (str index))))
        pages)]]))

(defn format-date
  [date]
  (letfn [(two-digits [n] (str (when (< n 10) "0") n))]
    (->> [(.getHours date) (.getMinutes date) (.getSeconds date)]
         (map two-digits)
         (string/join ":"))))

(rum/defc infos
  [position editor]
  (let [{:keys [filename time]} editor
        {:keys [current total]} (:counts position)]
    [:div.infos
     [:div.page
      (str "page "(inc current) " of " total)]
     (when (and (some? filename) (some? time))
       [:div.saved
        (str "last saved : " filename " at " (format-date time))])]))

(rum/defc properties
  [config position])


(rum/defc editor < s/size-listener-mixin rum/static
  [config position editor]
  (let [window-height (.-innerHeight js/window)
        window-width (.-innerWidth js/window)
        screen-ratio (:screen-ratio config)
        base-width (min window-width (/ window-height screen-ratio))
        preview-width (min (/ (* 8 base-width) 9)
                           (/ (* 4 window-width) 9))
        thumbnail-width (- (/ window-width 9) 19)]
    [:div.editor {:style {:grid-template-rows (str (/ (* 8 window-height) 9) "px auto")
                          :grid-template-columns (str (/ window-width 9) "px "
                                                   (/ (* 4 window-width) 9) "px auto")}}
     (thumbnails config position thumbnail-width)
     (preview config position preview-width)
     (infos position editor)]))
