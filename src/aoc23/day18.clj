(ns aoc23.day18
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample
"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

;;we're drawing a polygon.
;;then determining area.

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map (fn [s]
              (let [[d n color] (s/split s #" ")]
                {:dir (first d)
                 :n (parse-long n)
                 color (u/unwrap color)})))))


;;screen space coordinates.
(def dirs
  {\R [1 0]
   \L [-1 0]
   \D [0 1]
   \U [0 -1]})

(defn trace [xs]
  (reductions (fn [[x y] {:keys [dir n]}]
                (let [[dx dy] (dirs dir)
                      dx (* dx n)
                      dy (* dy n)]
                  [(+ x dx) (+ y dy)])) [0 0] xs))

(defn extents [xs]
  (reduce (fn [[l r] x] [(min l x) (max r x)]) [Long/MAX_VALUE Long/MIN_VALUE] xs))

(defn expand [xs]
  (->> (concat xs [(first xs)])
       (partition 2 1)
       (map (fn [[[x1 y1] [x2 y2]]]
              (let [dx (- x2 x1)
                    dy (- y2 y1)]
                (if (zero? dx)
                  (for [y (range y1 (+ y1 dy) (if (pos? dy) 1 -1))]
                    [x1 y])
                  (for [x (range x1 x2 (if (pos? dx) 1 -1))]
                    [x y1])))))
       (apply concat)))

;;lame.
(defn bounds [pairs]
  {:x (extents (map first pairs))
   :y (extents (map second pairs))})

(defn gather-points [xs]
  (->> xs
       (reduce (fn [{:keys [acc prev inside?] :as state} x]
                 ;;(println [state x])
                 (cond
                   (not prev) (assoc state :prev x :inside? true :acc 1)
                   inside?   (let [between (dec (- x prev))
                                   #_#__ (println [:accumulating between :from prev x])]
                               (if (zero? between)
                                 (assoc state :prev x :inside? true  :acc (+ acc 1))
                                 (assoc state :prev x :inside? false   :acc (+ acc between 1))))
                   :else (assoc state :prev x :inside? true  :acc (inc acc))))
               {:acc 0 :prev nil :inside? false})
       :acc))

(defn normalize [xs]
  (let [{:keys [x y]} (bounds xs)
        [l r] x
        [d u] y]
    (->> xs
         (map (fn [[x y]]
                [(- x l) (- y d)])))))

(defn hsweep [coords]
  (let [rows (->> (group-by second coords)
                  (sort-by key)
                  (mapv (fn [[k xs]]
                          [k (->> xs (map first) sort)])))]
    (for [[r xs] rows]
      [(gather-points xs) [r xs]])))

(defn solve1 [txt]
  (->> txt
       parse-input
       trace
       normalize
       expand
       hsweep
       (map first)
       (reduce +)))


(def svg-template
  "<svg width=\"1000\" height=\"1000\" xmlns=\"http://www.w3.org/2000/svg\">

  <polygon points=\"POINTS\" fill=\"transparent\" stroke=\"black\">

  </svg>")

(defn render [pts]
  (let [points (->> pts
                    normalize
                    (map (fn [[x y]]
                           (str x "," y)))
                    (s/join " "))]
    (s/replace svg-template "POINTS" points)))
