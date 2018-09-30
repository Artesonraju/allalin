(ns allalin.state
  (:require
    [clojure.string :as string]
    [cljs.reader :as reader]
    [cljs.core.async :refer [go-loop put! chan <! >!]]
    [allalin.position :as position]
    [allalin.config :as conf]
    [allalin.component :as comp]))

(def default-filename "allalin.edn")

; state
(defonce app-state (atom {:phase :loading
                          :config nil
                          :config-hash nil
                          :config-str nil
                          :position nil
                          :channel nil
                          :filename nil
                          :mode nil}))

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

(defn build-config!
  [file]
  (let [{:keys [edn content filename]} file]
    (swap! app-state
      (fn [state]
        (let [{:keys [position mode]} state]
          (-> state
              (dissoc :error)
              (assoc :config edn
                     :config-str (or content (str edn))
                     :mode (conf/mode mode)
                     :phase :loaded
                     :filename filename
                     :position (position/init-position edn position))
              (synchro)))))))

(defn preload-images!
  [images]
  (->> images
       (map #(set! (.-src (js/Image.)) (str "./images/" %)))
       (doall)))

(defn explain
  [problems]
  (println problems)
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

(defn init-config! [file]
  (let [edn (read-config (:content file))]
    (when (not (conf/valid edn))
      (throw (js/Error. (explain (conf/problems edn)))))
    (let [{:keys [config] :as state} (build-config! (assoc file :edn edn))]
      (preload-images! (conf/images config))
      (set! (. js/document -title) (:title config))
      (init-channel! state))))

(defn fetch-error [res]
  (js/Error. (str "Unable to load config.edn (" (.-status res)
                  " " (.-statusText res) ").")))
(defn error! [err]
  (swap! app-state (fn [state]
                     (assoc state :phase :error
                                  :error err))))

(defn set-config-error!
  [err]
  (set! (. js/document -title) "Allalin - Error")
  (error! err))

(defn set-config-loading!
  []
  (swap! app-state assoc :phase :loading
    :loading "Config loading...")
  (set! (. js/document -title) "Allalin - Loading..."))

(defn load-conf!
  []
  (set-config-loading!)
  (-> (js/fetch (str "./" default-filename))
      (.then (fn [r]
               (if (.-ok r)
                 r
                 (throw (fetch-error r)))))
      (.then (fn [r] (.text r)))
      (.then #(init-config! {:content %
                             :filename default-filename}))
      (.catch (fn [err] (set-config-error! err)))))

; curtain
(defn toggle-hide! []
  (swap! app-state update :hidden not))

; help
(defn toggle-help! []
  (swap! app-state update :help not))

; slide commands
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

(defn toggle-mode
  [state mode]
  (if (= mode (:mode state))
    (assoc state :mode :basic)
    (assoc state :mode mode)))

; print commands
(defn toggle-print! []
  (swap! app-state toggle-mode :print))

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

(defn toggle-notes! []
  (swap! app-state
    (fn [state]
      (let [timer @timer-state]
        (when (not (:active timer))
          (swap! timer-state assoc :active true)
          (js/setInterval timer-callback 1000))
        (toggle-mode state :notes)))))

(defn next-layout
  [layouts layout]
  (if (or (= layout (first layouts)) (nil? layout))
    (second layouts)
    (recur (rest layouts) layout)))

(defn next-layout! [layouts]
  (swap! app-state update-in [:notes :layout] #(next-layout layouts (or % :notes-only))))

(defn reset-timer! []
  (swap! timer-state assoc :seconds 0)
  (update-timer!))

;load and save commands
(def upload-chan (chan 1))

(def read-chan (chan 1))

(defn throw-err [e]
  (when (instance? js/Error e) (throw e))
  e)

(go-loop []
  (let [reader (js/FileReader.)
        file (<! upload-chan)]
    (try
      (set! (.-onload reader)
            (fn [c]
              (let [content (-> c .-target .-result js->clj)
                    title (.-name file)]
                (put! read-chan {:content content
                                 :filename title}))))
      (.readAsText reader file)
      (catch :default e
        (put! read-chan e)))
    (recur)))

(go-loop []
  (try
    (-> (throw-err (<! read-chan))
        (init-config!))
    (catch :default e
      (set-config-error! e)))
  (recur))

(defn load-file!
  [event]
  (let [target (.-currentTarget event)
        file (-> target .-files (aget 0))]
    (set! (.-value target) "")
    (set-config-loading!)
    (put! upload-chan file)))

(def storage-separator "/")

(defn save-storage! []
  (let [{:keys [config-str filename]} @app-state
        item (str filename storage-separator config-str)]
    (when (and (some? js/localStorage) (some? config-str) (some? filename))
      (js/localStorage.setItem "last" item))))

(def retrieve-chan (chan 1))

(go-loop []
  (try
    (let [item (<! retrieve-chan)
          index-sep (string/index-of item storage-separator)
          filename (subs item 0 index-sep)
          content (subs item (inc index-sep))]
      (init-config! {:filename filename
                     :content content}))
    (catch :default e
      (set-config-error! e)))
  (recur))

(defn retrieve-storage! []
  (when (some? js/localStorage)
    (set-config-loading!)
    (when-let [item (js/localStorage.getItem "last")]
      (put! retrieve-chan item))))