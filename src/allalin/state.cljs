(ns allalin.state
  (:require
    [clojure.string :as string]
    [cljs.reader :as reader]
    [allalin.position :as position]
    [allalin.config :as conf]
    [allalin.component :as comp]))

(defonce app-state (atom {:phase :loading
                          :config nil
                          :position nil
                          :mode nil}))

(defn reload! []
  (when (not (get-in @app-state [:config :disable-reload]))
    (swap! app-state assoc :phase :loading)
    true))

(defn build-config! [config]
  (swap! app-state
    (fn [state]
      (let [{:keys [position mode]} state]
        (-> state
            (dissoc :error)
            (assoc :config config
                   :mode (conf/mode config mode)
                   :phase :loaded
                   :position (position/init-position config position))))))
  ; return the new config
  (:config @app-state))

(defn preload-images!
  [images]
  (->> images
       (map #(set! (.-src (js/Image.)) (str "./images/" %)))
       (doall)))

(defn explain
  [problems]
  (str "Problems in config at : "
       (string/join ", " (map #(str (:in %)) (take 4 problems)))
       (when (> (count problems) 4) (str " (and " (- (count problems) 4) " more...)"))))

(defn read-config
  [s]
  (reader/read-string {:readers comp/edn-readers} s))

(defn init-config! [config]
  (when (not (conf/valid config))
    (throw #js {:message (explain (conf/problems config))}))
  (let [config (build-config! config)]
    (preload-images! (conf/images config))
    (set! (. js/document -title) (:title config))))

(defn fetch-error [res]
  (js/Error. (str "Unable to load config.edn (" (.-status res)
                  " " (.-statusText res) ").")))
(defn error! [err]
  (swap! app-state (fn [state]
                     (assoc state :phase :error
                                  :error err))))

(defn set-config-error! [err]
  (set! (. js/document -title) "Allalin - Error")
  (error! err))

(defn load-conf! []
  (when (reload!)
    (set! (. js/document -title) "Allalin - Loading...")
    (-> (js/fetch "./config.edn")
        (.then (fn [r]
                 (if (.-ok r)
                   r
                   (throw (fetch-error r)))))
        (.then (fn [r] (.text r)))
        (.then #(init-config! (read-config %)))
        (.catch (fn [err] (set-config-error! err))))))

(defn go-start! []
  (swap! app-state update :position (partial position/nth-page-position 0)))

(defn go-to-page! [index]
  (swap! app-state update :position (partial position/nth-page-position index)))

(defn go-previous! []
  (swap! app-state update :position position/dec-position))

(defn go-next! []
  (swap! app-state update :position position/inc-position))

(defn go-end! []
  (swap! app-state update :position position/last-position))
