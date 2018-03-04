(ns allalin.core
    (:require [rum.core :as rum]
              [cljs.reader :refer [read-string]]))

(enable-console-print!)

(defonce app-state (atom {:state :loading :errors []}))

(defonce _
  (-> (js/fetch "./config.edn")
    (.then (fn [r] (if (.-ok r)
                     r
                     (throw (js/Error. (.-statusText r))))))
    (.then (fn [r] (.text r)))
    (.then (fn [text] (swap! app-state assoc :config (read-string text) :state :loaded)))
    (.catch (fn [err] (swap! app-state (fn [state]
                                           (-> state
                                            (assoc :state :error)
                                            (update :errors conj err))))))))

(rum/defc app < rum/reactive []
          (let [state (rum/react app-state)]
            [:div.app-ratio
             [:div.app-page
              [:h1 (str state)]
              [:h3 "Edit this and watch it change!"]]
             [:footer "Bla bla bla"]]))

(rum/mount (app)
           (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)