(ns ^:figwheel-hooks allalin.core
  (:require
   [goog.dom :as gdom]
   [rum.core :as rum]
   [allalin.state :as state]
   [allalin.mode.basic :refer [basic]]))

(rum/defc loaded < rum/static
  [config mode position]
  (case mode
    :basic (basic config position)))

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

; app init

(defonce _ (state/load-conf!))

(defn key-handler
  [key-event]
  (when (contains? #{"r" "R"} (.-key key-event))
    (state/load-conf!)))

(def key-listener-mixin
  (letfn [(mount [state]
            (.addEventListener js/document
                     "keydown"
                     key-handler
                     false)
            (assoc state ::key-handler key-handler))
          (unmount [state]
            (.removeEventListener js/document
                                  "keydown"
                                  (::key-handler state)
                                  false))]
    {:did-mount mount
     :will-unmount unmount}))

(rum/defc app < rum/reactive key-listener-mixin
  []
  (let [{:keys [error phase config mode position]} (rum/react state/app-state)]
    [:div.fill
     (case phase
       :loading (loading)
       :error (err error)
       :loaded (loaded config mode position))]))

(defn mount [el]
  (rum/mount (app) el))

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element))
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

