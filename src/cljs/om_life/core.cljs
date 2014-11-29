(ns om-life.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [om-life.events :as e :refer [publish! event]]
            [sablono.core :as html :refer-macros [html]]))

(def seed
  #{[8 13] [9 13] [4 11] [6 13] [8 14] [11 14] [4 12] [5 13]
    [4 13] [14 14] [12 13] [15 14] [12 14] [14 13] [38 13]
    [37 13] [34 13] [35 14] [4 14] [34 14] [5 14] [32 13]
    [6 14] [27 12] [29 13] [27 11] [28 13] [31 14] [29 14]
    [37 14] [24 14] [27 14] [27 13] [41 14] [18 13] [20 14]
    [40 13] [18 14] [21 14] [17 14] [40 15] [17 13] [18 12]
    [23 14] [23 13] [21 13] [20 13]})

(defonce life (atom seed))
(defonce dragging? (atom false))
(defonce ticker (atom nil))
(defonce benchmarks (atom []))
(defonce history (atom []))

(defn change-cell
  ([x y]
     (swap! life (fn [cells]
                   (if (cells [x y])
                     (set (remove #{[x y]} cells))
                     (conj cells [x y])))))
  ([x y add?]
     (if add?
       (swap! life conj [x y])
       (swap! life #(set (remove #{[x y]} %))))))

(defn change [x y]
  (fn [e]
    (.preventDefault e)
    (change-cell x y)))

(defn drag-start [x y]
  (fn [e]
    (.preventDefault e)
    (reset! dragging? true)))

(defn drag-end [x y]
  (fn [e]
    (.preventDefault e)
    (reset! dragging? false)))

(defn drag-by [x y]
  (fn [e]
    (.preventDefault e)
    (if @dragging?
      (change-cell x y :add))))

(defn cell [{:keys [x y alive?]} owner]
  (reify
    om/IRender
    (render [_]
      (html
       [:td {:class (when alive? "alive")
             :style (when alive?
                      {:background-color (rand-nth ["#f00"
                                                    "#0f0"
                                                    "#00f"])})
             :on-click (change x y)
             :on-mouse-down (drag-start x y)
             :on-mouse-up (drag-end x y)
             :on-mouse-move (drag-by x y)}]))))

(defn publish-step-event! []
  (publish! (event ::step)))

(defn start [owner]
  (fn [e]
    (.preventDefault e)
    (js/clearInterval @ticker)
    (om/set-state! owner :running? true)
    (reset! ticker (.setInterval js/window publish-step-event! 80))))

(defn stop [owner]
  (fn [e]
    (.preventDefault e)
    (js/clearInterval @ticker)
    (om/set-state! owner :running? false)))

(defn game [world owner]
  (reify
    om/IInitState
    (init-state [_]
      {:height 30
       :width 50})
    om/IRenderState
    (render-state [_ {:keys [width height running?]}]
      (html
       [:div
        [:div.tools
         [:button {:on-click (fn [e]
                               (.preventDefault e)
                               (js/console.log (pr-str @life)))} "Print"]
         [:button {:on-click (fn [e]
                               (.preventDefault e)
                               (publish! (event ::rewind)))} "Rewind"]
         [:button {:on-click (fn [e]
                               (.preventDefault e)
                               (publish! (event ::step)))} "Step"]
         (if running?
           [:button {:on-click (stop owner)} "Stop"]
           [:button {:on-click (start owner)} "Start"])]
        [:table.game
         (for [h (range height)]
           [:tr
            (for [w (range width)]
              (om/build cell {:x w :y h :alive? (world [w h])}))])]]))))

(defn get-neighbors [p]
  ((apply juxt (for [a [-1 0 1]
                     b [-1 0 1]
                     :when (not= [0 0] [a b])]
                 (fn [[x y]] [(+ x a) (+ y b)]))) p))

(defn generation [cells]
  (set (for [[loc n] (frequencies (mapcat get-neighbors cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(defn step [_]
  (swap! history #(cons @life (take 999 %)))
  (swap! life generation))

(defn rewind [_]
  (when-let [prev (first @history)]
    (reset! life prev)
    (swap! history rest)))

(defn bench []
  (let [latest (take 1000 @benchmarks)]
    (/ (- (first latest) (last latest))
       (dec (count latest)))))

(defonce _
  (e/subscriptions
   [::step] step
   [::rewind] rewind))

(defn main []
  (om/root
   game
   life
   {:target (. js/document (getElementById "app"))}))

