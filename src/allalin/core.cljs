(ns ^:figwheel-hooks allalin.core
  (:require
   [goog.dom :as gdom]
   [goog.dom.fullscreen :as fs]
   [rum.core :as rum]
   [allalin.state :as state]
   [allalin.mode.basic :as basic]
   [allalin.mode.hidden :as hidden]
   [allalin.mode.print :as print]))

(defn get-app-element []
  (gdom/getElement "app"))

(defonce _ (state/load-conf!))

(defn toggle-fullscreen []
  (when (fs/isSupported)
    (if (fs/isFullScreen)
      (fs/exitFullScreen)
      (fs/requestFullScreen (get-app-element)))))

(def action-keys {state/load-conf! ["r" "R"]
                  toggle-fullscreen ["f" "F"]
                  state/toggle-hide! ["h" "H"]
                  state/to-print! ["p" "P"]
                  state/to-basic! ["s" "S"]})

(defn to-key-map [action-keys]
  (reduce (fn [acc [action keys]]
            (->> keys
                 (map #(vector % action))
                 (into {})
                 (merge acc)))
          {}
          action-keys))

(def keymaps
  (let [key-action (to-key-map action-keys)]
    {:basic (merge (to-key-map basic/action-keys)
                   key-action)
     :hidden key-action
     :print (merge (to-key-map print/action-keys)
                   key-action)}))

(defn key-handler
  [event]
  (let [mode (:mode @state/app-state)
        action (get-in keymaps [mode (.-key event)])]
    (when (some? action)
      (action))))

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

(rum/defc loaded < rum/static
  [config mode position print]
  (case mode
    :basic (basic/basic config position)
    :hidden (hidden/hidden)
    :print (print/print- config position print)))

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

(rum/defc app < rum/reactive key-listener-mixin
  []
  (let [{:keys [error phase config mode position print]} (rum/react state/app-state)]
    [:div.fill
     (case phase
       :loading (loading)
       :error (err error)
       :loaded (loaded config mode position print))]))

(defn mount [el]
  (rum/mount (app) el))

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

