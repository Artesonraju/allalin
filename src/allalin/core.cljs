(ns ^:figwheel-hooks allalin.core
  (:require
   [goog.dom :as gdom]
   [goog.dom.fullscreen :as fs]
   [rum.core :as rum]
   [allalin.state :as state]
   [allalin.slide :as s]
   [allalin.mode.basic :as basic]
   [allalin.mode.print :as print]
   [allalin.mode.notes :as notes]))

(defn get-app-element []
  (gdom/getElement "app"))

(defonce _ (state/load-conf!))

(defn toggle-fullscreen []
  (when (fs/isSupported)
    (if (fs/isFullScreen)
      (fs/exitFullScreen)
      (fs/requestFullScreen (get-app-element)))))

(defn curtain-tip
  []
  (s/tip ["C"] "toggle the curtain"))

(defn help-tip
  []
  (s/tip ["H"] "toggle help"))

(def always-actions [[state/toggle-help!  {:keys ["h" "H"]
                                           :tip (help-tip)}]])

(def slides-actions [[state/go-start! {:keys ["Home"]
                                       :tip (s/tip ["↖"] " to go to the first slide")}]
                     [state/go-next! {:keys ["Enter" "PageDown" "ArrowRight" "ArrowDown"]
                                      :tip (s/tip ["↵" "→" "↓"] "go to the next slide")}]
                     [state/go-previous! {:keys ["PageUp" "ArrowLeft" "ArrowUp"]
                                          :tip (s/tip ["→" "↑"] "go back to the previous slide")}]
                     [state/go-end! {:keys ["End"]
                                     :tip (s/tip ["↘"] "go to the last slide")}]])

(def loaded-actions [[state/toggle-hide!  {:keys ["c" "C"]
                                           :tip (curtain-tip)}]
                     [state/load-conf!    {:keys ["r" "R"]
                                           :tip (s/tip ["R"] "reload")
                                           :disable-key :disable-reload}]
                     [toggle-fullscreen   {:keys ["f" "F"]
                                           :tip (s/tip ["F"] "toggle fullscreen")}]])

(def modes-actions [[state/toggle-print! {:keys ["p" "P"]
                                          :tip (s/tip ["P"] "toggle print display")
                                          :disable-key :disable-print
                                          :sub-actions print/action-keys}]
                    [state/toggle-notes! {:keys ["n" "N"]
                                          :tip (s/tip ["N"] "toggle notes display")
                                          :disable-key :disable-notes
                                          :sub-actions notes/action-keys}]])

(def non-basic-actions [[state/to-basic!  {:keys ["s" "S"]
                                           :tip (s/tip ["S"] "go back to slide display")}]])

(defn to-key-map [action-keys]
  (reduce (fn [acc [f m]]
            (->> (:keys m)
                 (map #(vector % (assoc m :fn f)))
                 (into {})
                 (merge acc)))
          {}
          action-keys))

(def keymaps
  (let [[always slides loaded modes non-basic]
        (mapv to-key-map
              [always-actions slides-actions loaded-actions
               modes-actions non-basic-actions])]
    {:curtain (merge always loaded)
     :loading always
     :error always
     :loaded {:basic (merge always loaded modes slides)
              :print  (merge always loaded modes non-basic (to-key-map print/action-keys))
              :notes  (merge always loaded modes non-basic slides (to-key-map notes/action-keys))}}))

(defn available-actions [state]
  (let [{:keys [phase mode]} state
        phase (if (:hidden state) :curtain phase)]
    (get-in keymaps (if (= :loaded phase) [phase mode] [phase]))))

(defn get-action [state key]
  (get (available-actions state) key))

(defn to-help-list
  [action-keys]
  (reduce (fn [acc [f m]]
            (conj acc (assoc m :fn f
                               :sub-help (to-help-list (:sub-actions m)))))
          []
          action-keys))

(rum/defc help-action
  [state action]
  (let [current-action (get-action state (first (:keys action)))
        {:keys [disable-key sub-help]} action
        disabled (and (some? disable-key) (get-in state [:config disable-key]))
        invalid (or (nil? current-action)
                    (not= (:fn current-action) (:fn action)))]
    (when (not disabled)
      [:div.help-tip {:class (when invalid "disabled")}
       (:tip action)
       (when (seq sub-help)
         [:div.help-sub
            (map-indexed
              (fn [index action]
                (rum/with-key
                  (help-action state action)
                  (str index)))
              sub-help)])])))

(rum/defc help-
  [state]
  (let [{:keys [help]} state]
    [:div.pane.help (when help {:class "on"})
     (map-indexed
       (fn [index action]
         (rum/with-key
           (help-action state action)
           (str index)))
       (mapcat to-help-list
               [always-actions
                loaded-actions
                slides-actions
                non-basic-actions
                modes-actions]))]))


(rum/defc curtain
  [on]
  [:div.pane.curtain (when on {:class "on"})
   (curtain-tip)])

(rum/defc loading- < rum/static
  [state]
  (let [{:keys [loading phase]} state]
    [:div.pane.loading (when (= :loading phase) {:class "on"})
     (help-tip)
     [:div.pane-state (or loading "")]]))

(rum/defc err < rum/static
  [error]
  (js/console.log error)
  [:div.pane.loading
   [:div.pane-title "Error"]
   [:div.pane-state (.-message error)]])

(rum/defc loaded < rum/static
  [state]
  (let [{:keys [config mode position print notes]} state]
    (case mode
      :print (print/print- config position print)
      :notes (notes/notes config position notes)
      (basic/basic config position))))

(defn key-handler
  [event]
  (let [state @state/app-state
        action (get-action state (.-key event))
        disabled (get-in state [:config (:disable-key action)])
        f (:fn action)]
    (when (and f (not disabled))
      (f))))

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
  (let [{:keys [error phase] :as state} (rum/react state/app-state)]
    [:div.relative
     (curtain (:hidden state))
     (loading- state)
     (help- state)
     (case phase
       :error (err error)
       :loaded (loaded state)
       nil)]))


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

