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

(defonce world (atom seed))

(defn cell [{:keys [x y alive?]} _owner]
  (reify
    om/IRender
    (render [_]
      (html
       [:td {:class (if alive? "alive")
             :style {:width "5px" :height "5px"}}]))))

(defn game [world owner]
  (reify
    om/IInitState
    (init-state [_]
      {:height 30
       :width 50})
    om/IRenderState
    (render-state [_ {:keys [width height]}]
      (html
       [:table
        (for [h (range height)]
          [:tr
           (for [w (range width)]
             (om/build cell {:x w :y h :alive? (world [w h])}))])]))))

(defn get-neighbors [p]
  ((apply juxt (for [a [-1 0 1]
                     b [-1 0 1]
                     :when (not= [0 0] [a b])]
                 (fn [[x y]] [(+ x a) (+ y b)]))) p))

(defn iterate-world [cells]
  (set (for [[loc n] (frequencies (mapcat get-neighbors cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(def benchmarks (atom []))

(defn step [_]
  (swap! benchmarks #(cons (. (js/Date.) (getTime)) (take 999 %)))
  (swap! world iterate-world))

(defn bench []
  (let [latest (take 1000 @benchmarks)]
    (/ (- (first latest) (last latest))
       (dec (count latest)))))

(defn ticker []
  (.setInterval js/window #(publish! (event ::step)) 100))

(defn main []
  (om/root
   game
   world
   {:target (. js/document (getElementById "app"))})
  (e/subscriptions
   [::step] step))

