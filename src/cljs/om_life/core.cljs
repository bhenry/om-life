(ns om-life.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [om-life.events :as e :refer [publish! event]]
            [sablono.core :as html :refer-macros [html]]))

(def christmas-colors
  ["#f00" "#903" "#900" "#c33"
   "#660" "#3c8D0D" "#9c6" "#360"])

(def possible-colors
  christmas-colors)

(def brandon-henry
  #{[8 13] [9 13] [4 11] [6 13] [8 14] [11 14] [4 12] [5 13]
    [4 13] [14 14] [12 13] [15 14] [12 14] [14 13] [38 13]
    [37 13] [34 13] [35 14] [4 14] [34 14] [5 14] [32 13]
    [6 14] [27 12] [29 13] [27 11] [28 13] [31 14] [29 14]
    [37 14] [24 14] [27 14] [27 13] [41 14] [18 13] [20 14]
    [40 13] [18 14] [21 14] [17 14] [40 15] [17 13] [18 12]
    [23 14] [23 13] [21 13] [20 13]})

(def christmas-tree
  #{[15 14] [18 7] [25 3] [39 24] [18 8] [28 23] [29 10] [14 21] [21 9] [16 13] [10 23] [32 13] [18 9] [38 19] [4 26] [23 27] [26 26] [25 14] [32 19] [13 25] [13 20] [26 4] [16 11] [33 12] [27 7] [30 26] [34 13] [23 7] [7 24] [22 2] [11 19] [24 9] [29 21] [23 26] [22 1] [15 20] [30 22] [27 6] [23 1] [24 26] [18 29] [12 20] [35 16] [35 26] [20 19] [9 26] [18 15] [26 15] [12 19] [24 15] [26 16] [17 15] [14 18] [17 16] [19 19] [36 22] [28 26] [29 13] [34 16] [6 26] [17 27] [35 19] [11 26] [20 5] [20 21] [32 15] [24 16] [37 17] [21 8] [22 10] [29 23] [32 26] [23 28] [8 24] [39 25] [18 20] [27 22] [36 19] [15 25] [10 26] [16 12] [19 29] [24 1] [19 5] [30 13] [33 13] [5 26] [6 25] [36 17] [16 16] [39 26] [11 20] [17 29] [16 26] [31 26] [23 10] [31 13] [14 14] [18 14] [16 17] [38 24] [29 9] [23 29] [13 21] [14 25] [28 8] [34 26] [37 23] [37 26] [22 7] [33 19] [30 10] [35 21] [37 18] [26 6] [33 20] [30 14] [15 26] [21 29] [28 21] [19 21] [7 26] [34 19] [37 19] [13 19] [33 26] [19 7] [16 14] [5 25] [20 29] [12 25] [21 20] [19 6] [15 17] [34 21] [15 18] [14 20] [22 0] [24 2] [40 25] [17 11] [17 14] [38 26] [25 17] [26 5] [8 26] [22 29] [25 4] [18 10] [9 23] [21 3] [12 22] [15 13] [33 16] [31 11] [31 15] [11 22] [21 4] [36 26] [10 20] [25 2] [24 8] [32 11] [17 28] [23 0] [17 10]})

(def seeds {"xmas" christmas-tree
            "bhenry" brandon-henry})

(def seed (get seeds "xmas"))

(defonce life (atom seed))
(defonce history (atom [seed]))

(defonce dragging? (atom false))
(defonce ticker (atom nil))
(defonce benchmarks (atom []))

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
                      {:background-color (rand-nth possible-colors)})
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
       :width 50
       :chosen-seed "bhenry"})
    om/IRenderState
    (render-state [_ {:keys [width height running?]}]
      (html
       [:div#observable
        [:div.actions
         
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
              (om/build cell {:x w :y h :alive? (world [w h])}))])]
        [:div.tools
         [:button {:on-click (fn [e]
                               (.preventDefault e)
                               (js/console.log (pr-str @life)))} "Print"]
         [:button {:on-click (fn [e]
                               (.preventDefault e)
                               (publish! (event ::clear)))} "Clear"]
         [:button {:on-click (fn [e]
                               (.preventDefault e)
                               (js/console.log (pr-str (om/get-state owner :chosen-seed)))
                               (publish!
                                (event ::reseed
                                       {:seed (om/get-state owner :chosen-seed)})))} "Reseed"]
         [:select {:on-change (fn [e]
                                (om/set-state! owner :chosen-seed
                                               (.-value (.-target e))))}
          (for [[k _] (sort-by first seeds)]
            [:option {:value (name k)} (name k)])]]]))))

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

(defn clear [_]
  (reset! life #{}))

(defn reseed [{s :seed}]
  (reset! life (get seeds s seed)))

(defn bench []
  (let [latest (take 1000 @benchmarks)]
    (/ (- (first latest) (last latest))
       (dec (count latest)))))

(defonce _
  (e/subscriptions
   [::step] step
   [::rewind] rewind
   [::clear] clear
   [::reseed] reseed))

(defn main []
  (om/root
   game
   life
   {:target (. js/document (getElementById "app"))}))

