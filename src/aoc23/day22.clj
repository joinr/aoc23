(ns aoc23.day22
  (:require [aoc23.util :as u]
            [clojure.string :as s]
            [aoc23.quilpatch]
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
          [r g b] color
          [sw sh sd :as sv] [(* 100 w) (* 100  h) (* 100  d)]
          [hw hh hd] (mapv #(/ % 2.0) sv)]
      (q/with-fill [r g b]
        (q/with-translation [(+ (* 100 x) hw)
                             (+ (* 100 y) hh)
                             (+ (* 100 z) hd)]
          (q/box sw sh sd)
          (q/with-fill [255 255 255]
            (q/text (str id) -100 -100 100 100)))))))

(defn draw-ground [{:keys [w h d]}]
  (q/with-fill [128 128 128]
    (q/with-translation  [0 0 100]
      (q/with-rotation [(q/radians 180) 1 0 0]
        (q/rect  (* w -500) (* h -500) (* w 1000) (* h 1000) #_(* d 100))))))

(defn draw-axes [_]
  (q/with-translation  [0 0 0]
    (q/with-stroke [255 0 0]
      (q/line 0 0 0 1000 0 0))  ;x red
    (q/with-stroke [0 255 0]
      (q/line 0 0 0 0 1000 0)   ;y green
    (q/with-stroke [0 0 255]
      (q/line 0 0 0 0 0 1000))))) ;;z blue

(defn render [ctx]
  (q/defsketch boxrender
      :renderer :p3d
      :size [1920 1080]
      :setup #(assoc ctx :navigation-3d {})
      :draw #(do (q/background 0 0 0)
                 (q/lights)
                 (draw-axes %)
                 #_(q/with-fill [100 100 100]
                   (q/box 500))
                 (draw-boxes %)
                 (draw-ground %))
      :update (fn [x] x)
      :middleware [m/fun-mode m/navigation-3d-flying]))


;;algorithms.
;;we can scan from the bottom up.

;;build connectivity graph
;; adjacency defined by vertical support.
;; weights are distance?
;; root/start is the ground.
;; walk the graph from the root.

;;walk a level at a time to see who is directly above us.
;;translate them down until they are in the supported set.

(defn intersects-z [z {:keys [start stop]}]
  (let [z0 (start 2)
        z1 (stop 2)]
    (<= z0 z z1)))

(defn xy-bounds [bx]
  [(bx :start) (bx :stop)])

;;reuse day5
(defn intersection [[lx1 lx2 :as l] [rx1 rx2 :as r]]
  (if (> lx1 rx1)
    (recur r l)
    (when-not (< lx2 rx1) ;;----- [    ]
      (let [left-overlap  (and (<= rx1 lx2) (>= rx1 lx1)) ;;---[----
            right-overlap (and (<= rx2 lx2) (>= rx2 lx1))] ;;-----]--
        (cond
          (and left-overlap right-overlap) ;;     ---[--]---
          (if (not= lx1 lx2 rx1 rx2)
            r ;;overlapping segment.
            :vertical)
          left-overlap                     ;;     -----[----   ]
          [rx1 lx2]
          right-overlap                    ;;[    -----]----
          [lx1 rx2]
          :else
          (when (and (<= rx1 lx1) (>= rx2 lx2)) l)))))) ;;[    -----    ]

(defn segments [b]
  (let [[x0 y0 z0] (b :start)
        [x1 y1 z1] (b :stop)]
    [[x0 x1]
     [y0 y1]
     [z0 z1]]))

;;is box b1 potentially supported by box b2?
;;is there any overlap in x or y?
(defn over? [b1 b2]
  (let [[xs0 ys0 _] (segments b1)
        [xs1 ys1 _] (segments b2)
        x-int (intersection xs0 xs1)
        y-int (intersection ys0 ys1)]
    (when (and x-int y-int)
      [x-int y-int])))

(defn boxes-at [ctx z]
  (->> ctx
       :boxes
       (filter #(intersects-z z %))))

;;if we have a supported set of boxes,
;;sorted by z-max.

;;we can look at a block
;;to see if it can be added to the
;;supported set.

;;criteria:
;;is the block over any member of
;;the support?
;;if more than one candidate,
;;choose the candidate with the
;;maximum z extent.
;;translate the block to be
;;1 above the maximum z extent.
;;add the block to the supported set.

;;if there's no candidate, then it falls
;;to the ground.
(defn find-supports [supported box]
  (let [zmax (atom nil)]
    (->> supported
         (filter (fn [candidate]
                   (when (over? box candidate)
                     (swap! zmax (fn [x] (if x x (candidate :zmax))))
                     true)))
         (take-while #(= (% :zmax) @zmax)))))

(defn drop-by [height {:keys [start stop] :as box}]
  (let [start (update start 2 - height)
        stop  (update stop  2 - height)
        zmax (stop 2)]
    (assoc box :start start :stop stop :zmax zmax)))

(defn stack [supported box]
  (if-let [candidates (seq (find-supports supported box))]
    (let [candidate (first candidates)
          height (:zmax candidate)
          z      (-> box :start (nth 2))
          delta  (- z height 1)]
      (conj supported (-> (drop-by delta box)
                          (assoc :over (mapv :id candidates)))))
    (conj supported (-> (drop-by (-> box :start (nth 2) dec) box)
                        (assoc :over [:ground])))))

(defn compare-boxes [l r]
  (let [res (compare (r :zmax) (l :zmax))]
    (if (zero? res)
      (compare (r :id) (l :id))
      res)))

(defn stack-all [boxes]
  (->> boxes
       (reduce stack
               (sorted-set-by compare-boxes))))

(defn stack-ctx [ctx] (update ctx :boxes stack-all))

;;if box has more than one parent, or no children, it can
;;be possibly be removed.

;;If I take away a node,
;; if there are children,
;; and any of the children have only one source [me]?
;;then I can't possibly be removed.

(defn nebs [ctx]
  (->> ctx
       :boxes
       (reduce (fn [acc {:keys [id over] :as box }]
                 (let [gr (-> acc
                              (assoc-in [:nodes id] (dissoc box :over :color))
                              (assoc-in [:sources id] (into {} (for [chld over]
                                                               [chld 1])))
                              (update-in [:sinks id] (fn [x]
                                                         (or x {}))))]
                   (reduce (fn [acc chld]
                             (update-in acc [:sinks chld]
                                        (fn [x] (assoc x id 1)))) gr over)))
               {:nodes {:ground nil} :sinks {} :sources {}})))

(defn possible? [gr nd]
  (let [children (get-in gr [:sinks nd])]
    (if (empty? children) true
        (->> children
             keys
             (map #(get-in gr [:sources %]))
             (map count)
             (every? #(> % 1))))))

;;we can work in topological order....
;;naive solution is to scan all of the nodes.

;;or just go in z-order.
;;cache results of possible.
(defn feasible [gr]
  (let [can? (u/memo-1 (fn [nd] (possible? gr nd)))]
    (->> (-> gr :nodes (dissoc :ground))
         vals
         (sort-by (comp - :zmax))
         (map :id)
         (filter can?)
         count)))
