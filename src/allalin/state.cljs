(ns allalin.state
  (:require
    [clojure.string :as string]
    [cljs.reader :as reader]
    [allalin.position :as position]
    [allalin.config :as conf]
    [allalin.component :as comp]))

; state
(defonce app-state (atom {:phase :loading
                          :config nil
                          :config-hash nil
                          :position nil
                          :channel nil
                          :mode nil}))

(defn reload! []
  (when (not (get-in @app-state [:config :disable-reload]))
    (swap! app-state assoc :phase :loading)
    true))

(defn synchro
  [state]
  (let [{:keys [config config-hash channel]} state
        h (hash config)]
    (if (or (not (:synchro config)) (= h config-hash) (not js/BroadcastChannel))
      (assoc state :channel nil)
      (do
        (when (some? channel) (.close channel))
        (assoc state :config-hash h
                     :channel (js/BroadcastChannel. (str h)))))))

(defn build-config! [config]
  (swap! app-state
    (fn [state]
      (let [{:keys [position mode]} state]
        (-> state
            (dissoc :error)
            (assoc :config config
                   :mode (conf/mode config mode)
                   :phase :loaded
                   :position (position/init-position config position))
            (synchro))))))

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

(defn post
  [state]
  (let [{:keys [position channel]} state
        {:keys [current stock]} (:counts position)]
    (when (some? channel)
      (.postMessage channel (clj->js {:current current
                                      :stock stock})))))

(defn message-handler
  [event]
  (let [{:keys [request current stock]} (js->clj (.-data event) :keywordize-keys true)]
    (if request
      (post @app-state)
      (swap! app-state
        (fn [state]
          (let [counts (-> state :position :counts)]
            (if (and (= current (:current counts)) (= stock (:stock counts)))
              state
              (update state :position #(position/nth-position current stock %)))))))))

(defn init-channel!
  [state]
  (let [channel (:channel state)]
    (when channel
      (set! (.-onmessage channel) message-handler)
      (.postMessage channel (clj->js {:request true})))))

(defn init-config! [config]
  (when (not (conf/valid config))
    (throw #js {:message (explain (conf/problems config))}))
  (let [{:keys [config] :as state} (build-config! config)]
    (preload-images! (conf/images config))
    (set! (. js/document -title) (:title config))
    (init-channel! state)))

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

; basic commands
(defn to-basic! []
  (swap! app-state assoc :mode :basic))

(defn go-start! []
  (post (swap! app-state update :position (partial position/nth-position 0))))

(defn go-to-page! [index]
  (post (swap! app-state update :position (partial position/nth-position index))))

(defn go-previous! []
  (post (swap! app-state update :position position/dec-position)))

(defn go-next! []
  (post (swap! app-state update :position position/inc-position)))

(defn go-end! []
  (post (swap! app-state update :position position/last-position)))

; hidden commands
(defn toggle-hide! []
  (swap! app-state (fn [s]
                     (assoc s :mode (or (:hidden s) :hidden)
                              :hidden (or (:mode s) :basic)))))

; print commands
(defn to-print! []
  (swap! app-state assoc :mode :print))

(defn add-print-margin! [amount]
  (swap! app-state update-in [:print :margin] #(max (+ % amount) 0)))

; notes commands
(defonce timer-state (atom {:active false
                            :seconds 0}))

(defn update-timer!
  []
  (let [ts @timer-state]
    (when-let [update-fn (:update ts)]
      (update-fn (:seconds ts)))))

(defn timer-callback
  []
  (swap! timer-state update :seconds inc)
  (update-timer!))

(defn to-notes! []
  (swap! app-state
    (fn [state]
      (let [timer @timer-state]
        (when (not (:active timer))
          (swap! timer-state assoc :active true)
          (js/setInterval timer-callback 1000))
        (assoc state :mode :notes)))))

(defn next-layout
  [layouts layout]
  (if (= layout (first layouts))
    (next layouts)
    (recur (rest layouts) layout)))

(defn next-layout! [layouts]
  (swap! app-state update-in [:notes :layout] next-layout layouts))

(defn reset-timer! []
  (swap! timer-state assoc :seconds 0)
  (update-timer!))