(ns allalin.mode.notes
  (:require
    [rum.core :as rum]
    [allalin.slide :as s]
    [allalin.state :as state]))

(def layouts (apply concat (repeat [:horizontal :notes-only :vertical])))

(def action-keys (merge s/action-keys
                        {(partial state/next-layout! layouts) ["m" "M"]
                         state/reset-timer! ["t" "T"]}))

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

(rum/defc notes-only < rum/static
  [config position timers]
  (let [{:keys [seconds now]} timers
        {:keys [counts]} position
        notes (-> config :pages (nth (:current counts)) :notes)]
    [:div.notes
     [:div.notes-only-header
      [:div
       (timer seconds)]
      [:div
       (page counts)]
      [:div
       (hour now)]]
     [:div.notes-body
      [:div.notes-caption (if (seq notes) "NOTES" "NO NOTES")]
      (map-indexed (fn [index note]
                     [:div {:key index} note])
                   notes)]]))

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
      ;:horizontal (notes-only config position)
      ;:vertical (notes-only config position)
      (notes-only config position timers))))
