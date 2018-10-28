(ns allalin.mode.notes
  (:require
    [rum.core :as rum]
    [allalin.position :as position]
    [allalin.slide :as s]
    [allalin.state :as state]))

(def layouts (apply concat (repeat [:notes-only :notes-n-slides])))

(def action-keys [[(partial state/next-layout! layouts)
                   {:keys ["m" "M"]
                    :tip (s/tip ["M"] "switch notes display mode")}]
                  [state/reset-timer!
                   {:keys ["t" "T"]
                    :tip (s/tip ["T"] "reset timer")}]])

(defn two-digits [n] (if (> n 9) (str n) (str "0" n)))

(defn format-timer [seconds]
  (let [h (quot seconds 3600)
        m (mod (quot seconds 60) 60)
        s (mod seconds 60)]
    (str (two-digits h) ":" (two-digits m) ":" (two-digits s))))

(defn format-date [date]
  (str (.getHours date) ":" (two-digits (.getMinutes date))))

(defn format-page [counts]
  (str (inc (:current counts)) "/" (:total counts)))

(rum/defc timer < rum/static
  [seconds]
  [:div.notes-timer
   [:div.notes-caption "TIMER"]
   [:div.notes-value (format-timer seconds)]])

(rum/defc page < rum/static
  [counts]
  [:div.notes-timer
   [:div.notes-caption "PAGE"]
   [:div.notes-value (format-page counts)]])

(rum/defc hour < rum/static
  [date]
  [:div.notes-timer
   [:div.notes-caption "HOUR"]
   [:div.notes-value (format-date date)]])

(rum/defc counters < rum/static
  [position timers]
  (let [{:keys [seconds now]} timers
        {:keys [counts]} position]
    [:div.notes-counters
      [:div
       (timer seconds)]
      [:div
       (page counts)]
      [:div
       (hour now)]]))

(rum/defc body < rum/static
  [config position]
  (let [notes (-> config :pages (nth (-> position :counts :current)) :notes)]
    [:div.notes-body
     [:div.notes-caption (if (seq notes) "NOTES" "NO NOTES")]
     (map-indexed (fn [index note]
                    [:div {:key index} note])
                  notes)]))

(rum/defc notes-only < rum/static
  [config position timers]
  [:div.notes-only
   (counters position timers)
   (body config position)])

(defn current-slide
  [config position width]
  (let [screen-ratio (:screen-ratio config)
        {:keys [parts counts]} position
        {:keys [pages default]} config
        current (:current counts)
        [page part] ((juxt (partial nth pages) (partial nth parts)) current)
        props {:default default :counts counts}]
    [:div.notes-n-slides-current
     [:div {:style {:height (* width screen-ratio)
                    :width width}}
      (s/slide page config part props width)]]))

(defn next-slide
  [config position width]
  (let [next-position (position/inc-position position)]
    (if (= position next-position)
      [:div.notes-n-slides-end.notes-bg
       "END"]
      (let [screen-ratio (:screen-ratio config)
            height (* width screen-ratio)
            {:keys [parts counts]} next-position
            {:keys [pages default]} config
            current (:current counts)
            [page part] ((juxt (partial nth pages) (partial nth parts)) current)
            props {:default default :counts counts}]
        [:div.notes-n-slides-next
         [:div {:style {:height height
                        :width width
                        :flex-basis width
                        :font-size height}}
          (s/slide page config part props width)]]))))

(rum/defc notes-n-slides < s/size-listener-mixin rum/static
  [config position timers]
  (let [window-height (.-innerHeight js/window)
        window-width (.-innerWidth js/window)
        screen-ratio (:screen-ratio config)
        base-width (min window-width (/ window-height screen-ratio))
        current-width (/ (* base-width 2) 3)
        next-max-height (/ (* window-height 5) 11)
        next-max-width (/ window-width 3)
        next-height (min (* next-max-width screen-ratio) next-max-height)
        next-width (/ next-height screen-ratio)
        counters-height (- (/ (* window-height 2) 3) next-height)
        large-width (max current-width (/ (* window-width 5) 9))]
    [:div.notes-n-slides {:style {:grid-template-rows (str counters-height "px " next-height "px auto")
                                  :grid-template-columns (str large-width "px auto")}}
     [:div.notes-n-slides-bg-current.notes-bg
      [:div
       "CURRENT"]]
     (current-slide config position current-width)
     [:div.notes-n-slides-bg-next.notes-bg
      [:div
       "NEXT"]]
     (next-slide config position next-width)
     (counters position timers)
     (body config position)]))

(def timer-mixin
  {:did-mount (fn [state]
                (let [seconds-atom (::seconds state)
                      callback (fn [s]
                                 (reset! seconds-atom s))]
                  (reset! seconds-atom (:seconds @state/timer-state))
                  (swap! state/timer-state assoc :update callback)
                  state))
   :will-unmount (fn [state]
                   (swap! state/timer-state dissoc :update)
                   state)})


(rum/defcs notes < (rum/local 0 ::seconds) timer-mixin
  [state config position notes]
  (let [seconds-atom (::seconds state)
        timers {:seconds @seconds-atom
                :now (js/Date.)}]
    (case (:layout notes)
      :notes-n-slides (notes-n-slides config position timers)
      (notes-only config position timers))))
