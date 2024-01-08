(ns aoc23.day22
  (:require [aoc23.util :as u]
            [clojure.string :as s]
            [quil.core :as q]
            [quil.middleware :as m]))

(def sample
"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn extents [xs]
  (reduce (fn [[l r] x] [(min l x) (max r x)]) [Long/MAX_VALUE Long/MIN_VALUE] xs))

;;lame.
(defn bounds [coords]
  {:x (extents (map first coords))
   :y (extents (map second coords))
   :z (extents (map #(nth % 2) coords))})

(defn add-bounds [{:keys [boxes] :as ctx}]
  (let [bnds (bounds (map :start boxes))
        [[x1 x2] [y1 y2] [z1 z2]] [(bnds :x) (bnds :y) (bnds :z)]]
    (assoc ctx :bounds bnds
           :w (- x2 x1)
           :h (- y2 y1)
           :d (- z2 z1))))

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map (fn [ln]
              (-> ln (s/replace "~" ",") u/read-as-vector)))
       (mapv #(mapv vec (partition 3 %)))
       (map-indexed (fn [idx [[x1 y1 z1] [x2 y2 z2] :as coords]]
                      {:id idx :start (coords 0) :stop (coords 1)
                       :w (inc  (- x2 x1))
                       :h (inc (- y2 y1))
                       :d (inc (- z2 z1))
                       :color [(rand-int 256) (rand-int 256) (rand-int 256)]}))
       (assoc {} :boxes)
       add-bounds))

(defn draw-boxes [{:keys [boxes]}]
  (doseq [{:keys [id start w h d color]} boxes]
    (let [[x y z] start
          [r g b] color]
      (q/with-fill [r g b]
        (q/with-translation [(* 100 x) (* 100 y) (* 100 z)]
          (q/box (* 100 w) (* 100  h) (* 100  d)))))))

(defn draw-ground [{:keys [w h d]}]
  (q/with-fill [255 255 255]
    (q/with-translation  [0 0 0]s
      (q/rect 0 0 w h))))


(defn render [ctx]
  (q/defsketch boxrender
      :renderer :p3d
      :size [1000 1000]
      :setup #(assoc ctx :navigation-3d {})
      :draw #(do (q/background 0 0 0)
                 (q/lights)
                 #_(q/with-fill [100 100 100]
                   (q/box 500))
                 (draw-boxes %)
                 #_(draw-ground %))

      :update (fn [x] x)
      :middleware [m/fun-mode m/navigation-3d]))

