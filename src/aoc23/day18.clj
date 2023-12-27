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
                 :color (u/unwrap color)})))))


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

(defn abs [x]
  (if (neg? x) (- x) x))
(defn shoelace [xys]
  (-> (->> (concat xys [(first xys)])
           (partition 2 1)
           (reduce (fn [[l r] [[x1 y1] [x2 y2]]] ;;determinants
                     [(+ l (* x1 y2))
                      (+ r (* x2 y1))]) [0 0])
           (apply -))
      abs #_Math/abs ;;precision screwed us.
      (/ 2)))

;;ugh screw this.
;;shoelace gives us the area of the interior of the polygon.
;;pick relates that to the integer constraints.

;;A=I + (B / 2) - 1
;;we know B, we can compute A, we don't know I.
;;so I = A - (B / 2) + 1
;;total = I + B
;;I + B = A - (B / 2) + 1 + B
;;I + B = A + B - (B / 2) + 1
;;I + B = A + (B / 2) + 1


;;need to compute perimeter without expanding points.
(defn solve1 [txt]
  (let [ins (->> txt
                 parse-input)
        pts (trace ins)
        a (shoelace pts)
        b (->> ins (map :n) (reduce +))
        i (+ 1 (- a (/ b 2)))]
    (+ i b)))


;;part2

(defn read-number [txt]
  (->>(subs txt 1 (dec (count txt)))
      (str "0x")
      read-string))

(defn read-dir [txt]
  (case (nth txt (dec (count txt)))
    \0 \R
    \1 \D
    \2 \L
    \3 \U))

(defn parse-input2 [txt]
  (->> txt
       parse-input
       (map (fn [{:keys [color]}]
              {:dir (read-dir color)
               :n   (read-number color)}))))

(defn solve2 [txt]
  (let [ins (->> txt
                 parse-input2)
        pts (trace ins)
        a (shoelace pts)
        b (->> ins (map :n) (reduce +))
        i (+ 1 (- a (/ b 2)))]
    (+ i b)))


;;failed raycasting solution (works on test case)
;;revisit.
;; (defn extents [xs]
;;   (reduce (fn [[l r] x] [(min l x) (max r x)]) [Long/MAX_VALUE Long/MIN_VALUE] xs))

;; (defn expand [xs]
;;   (->> (concat xs [(first xs)])
;;        (partition 2 1)
;;        (map (fn [[[x1 y1] [x2 y2]]]
;;               (let [dx (- x2 x1)
;;                     dy (- y2 y1)]
;;                 (if (zero? dx)
;;                   (for [y (range y1 (+ y1 dy) (if (pos? dy) 1 -1))]
;;                     [x1 y])
;;                   (for [x (range x1 x2 (if (pos? dx) 1 -1))]
;;                     [x y1])))))
;;        (apply concat)))

;; ;;lame.
;; (defn bounds [pairs]
;;   {:x (extents (map first pairs))
;;    :y (extents (map second pairs))})

;; (defn gather-points [xs]
;;   (->> xs
;;        (reduce (fn [{:keys [acc prev inside?] :as state} x]
;;                  ;;(println [state x])
;;                  (cond
;;                    (not prev) (assoc state :prev x :inside? true :acc 1)
;;                    inside?   (let [between (dec (- x prev))
;;                                    #_#__ (println [:accumulating between :from prev x])]
;;                                (if (zero? between)
;;                                  (assoc state :prev x :inside? true  :acc (+ acc 1))
;;                                  (assoc state :prev x :inside? false   :acc (+ acc between 1))))
;;                    :else (assoc state :prev x :inside? true  :acc (inc acc))))
;;                {:acc 0 :prev nil :inside? false})
;;        :acc))

;; (defn normalize [xs]
;;   (let [{:keys [x y]} (bounds xs)
;;         [l r] x
;;         [d u] y]
;;     (->> xs
;;          (map (fn [[x y]]
;;                 [(- x l) (- y d)])))))

;; ;;works on sample, fails on full.  crapping out to picks...
;; (defn hsweep [coords]
;;   (let [rows (->> (group-by second coords)
;;                   (sort-by key)
;;                   (mapv (fn [[k xs]]
;;                           [k (->> xs (map first) sort)])))]
;;     (for [[r xs] rows]
;;       [(gather-points xs) [r xs]])))

;; (defn solve1 [txt]
;;   (->> txt
;;        parse-input
;;        trace
;;        normalize
;;        expand
;;        hsweep
;;        (map first)
;;        (reduce +)))
