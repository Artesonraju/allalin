(ns allalin.core
  (:require [rum.core :as rum]
            [cljs.reader :refer [read-string]]))

(enable-console-print!)

; default values

(def default-config {:global {:screen-ratio 0.625
                              :background {:color "#fff"}
                              :title "Allalin - Presentation framework"}
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


; utils

(defn or-default
  ([config keys] (get-in config keys (get-in default-config keys)))
  ([page config keys] (get-in page keys (or-default config keys))))

(defn calc-vh [size ratio]
  (str "calc(" size "vh / " ratio ")"))

(defn calc-vw [size ratio]
  (str "calc(" size "vw * " ratio ")"))


; state

(defonce app-state (atom {:state :loading
                          :current 0}))

; interactions

(defn config-default-pages [config]
  (cond-> config
    (= 0 (count (:pages config))) (assoc :pages (:pages default-config))))


(defn init-config [config]
  (set! (. js/document -title) (or-default config [:global :title]))
  (swap! app-state
    (fn [state]
      (let [current (:current state)
            config (config-default-pages config)
            pages (:pages config)]
        (-> state
            (dissoc :error)
            (assoc :config config
                   :state :loaded
                   :current (min current (- (count pages) 1))))))))

(defn fetch-error [r]
  (js/Error. (str "Unable to load config.edn (" (.-status r) " " (.-statusText r) ").")))

(defn config-error [err]
  (set! (. js/document -title) "Allalin - Error")
  (swap! app-state (fn [state]
                       (assoc state :state :error
                              :error err))))

(defn load-conf []
         (set! (. js/document -title) "Allalin - Loading...")
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

(defn first-page []
  (swap! app-state assoc :current 0))

(defn previous-page []
  (swap! app-state update :current #(max 0 (- % 1))))

(defn next-page []
  (swap! app-state (fn [state]
                     (let [pages (or-default (:config state) [:pages])
                           last (- (count pages) 1)]
                       (update state :current #(min last (+ % 1)))))))

(defn last-page []
  (swap! app-state (fn [state]
                     (let [pages (or-default (:config state) [:pages])]
                       (assoc state :current (- (count pages) 1))))))

(defn key-handler [key-event]
  (condp contains? (.-key key-event)
    #{"r" "R"} (load-conf)
    #{"f" "F"} (first-page)
    #{"Backspace" "PageUp" "ArrowLeft" "ArrowUp" "p" "P"} (previous-page)
    #{"Enter" " " "PageDown" "ArrowRight" "ArrowDown" "n" "N"} (next-page)
    #{"End"} (last-page)
    ())) ; default : no-op

; initialisation

(defonce _ (load-conf))


; components

(rum/defc header < rum/static
  [page config]
  (let [height (or-default page config [:header :height])
        ratio (or-default config [:global :screen-ratio])
        base-height (calc-vw height ratio)
        base-width (calc-vh 100 ratio)]
    [:div.header-wrapper {:style {:background-color (or-default page config [:header :background-color])
                                  :color (or-default page config [:header :color])
                                  :flex-basis base-height}};}}
     [:header]
     [:style
      (str "header {height: " base-height "; }
@media (min-width: " (/ 100 ratio)  "vh) { header {height: 100%; width: " base-width "; }}")]]))

(rum/defc footer < rum/static
  [page config]
  (let [height (or-default page config [:footer :height])
        ratio (or-default config [:global :screen-ratio])
        base-height (calc-vw height ratio)
        base-width (calc-vh 100 ratio)]
    [:div.footer-wrapper {:style {:background-color (or-default page config [:footer :background-color])
                                  :color (or-default page config [:footer :color])
                                  :flex-basis base-height}};}}
     [:footer]
     [:style
      (str "footer {height: " base-height "; }
@media (min-width: " (/ 100 ratio)  "vh) { footer {height: 100%; width: " base-width "; }}")]]))

(rum/defc left < rum/static
  [page config]
  (let [width (or-default page config [:left :width])
        ratio (or-default config [:global :screen-ratio])
        base-width (calc-vh width ratio)]
    [:div.aside-wrapper {:style {:background-color (or-default page config [:left :background-color])
                                 :color (or-default page config [:left :color])
                                 :flex-basis base-width}};}}
     [:aside.left]
     [:style
      (str "aside.left {width: 100%; }
@media (min-width: " (/ 100 ratio)  "vh) { aside.left {width: " base-width "; }}")]]))

(rum/defc right < rum/static
  [page config]
  (let [width (or-default page config [:right :width])
        ratio (or-default config [:global :screen-ratio])
        base-width (calc-vh width ratio)]
    [:div.aside-wrapper {:style {:background-color (or-default page config [:right :background-color])
                                 :color (or-default page config [:right :color])
                                 :flex-basis base-width}};}}
     [:aside.right]
     [:style
      (str "aside.right {width: 100%; }
@media (min-width: " (/ 100 ratio)  "vh) { aside.right {width: " base-width "; }}")]]))

(rum/defc main < rum/static
  [page config]
  (let [width (- 100
                  (or-default page config [:left :width])
                  (or-default page config [:right :width]))
        ratio (or-default config [:global :screen-ratio])]
    [:main {:style {:flex-basis (calc-vh width ratio)}}]))

(rum/defc middle < rum/static
  [page config]
  (let [height (- 100
                  (or-default page config [:header :height])
                  (or-default page config [:footer :height]))
        ratio (or-default config [:global :screen-ratio])]
    [:div.middle {:style {:flex-basis (calc-vw height ratio)}}
     (left page config)
     (main page config)
     (right page config)]))

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
  [page config]
  [:div.root.fill
   (header page config)
   (middle page config)
   (footer page config)])

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

(rum/defc app
  < rum/reactive
    key-listener-mixin
  []
  (let [{:keys [state config current error]} (rum/react app-state)
        page (nth (or-default config [:pages]) current)
        ratio (or-default config [:global :screen-ratio])
        base-width (calc-vh 100 ratio)]
    [:div.fill
     (case state
       :loading (loading)
       :error (err error)
       :loaded (loaded page config))
     [:style
      (str "#app {font-size: 100vw; }
@media (min-width: " (/ 100 ratio)  "vh) { #app {font-size: " base-width "; }}")]]))



(rum/mount (app) (. js/document (getElementById "app")))

(defn on-js-reload [])
;; optionally touch your app-state to force rerendering depending on
;; your application
;; (swap! app-state update-in [:__figwheel_counter] inc)