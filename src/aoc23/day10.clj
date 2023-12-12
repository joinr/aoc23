(ns aoc23.day10
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample
".....
.S-7.
.|.|.
.L-J.
.....")

(def complex
"7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

;;destinations for four cardinal directions.
(def lefts  #{\-  \L  \F \S})
(def rights #{\-  \J  \7 \S} )
(def ups    #{\|  \F  \7 \S} )
(def downs  #{\|  \J  \L \S})

;;leaving from this char, which chars
;;are valid in each of the directions?
(def valid
  {\|  {:u ups
        :d downs}
   \-  {:l lefts
        :r rights}
   \L  {:u ups
        :r rights}
   \F  {:d downs
        :r rights}
   \J  {:u ups
        :l lefts}
   \7  {:d downs
        :l lefts}
   \S  {:u ups
        :d downs
        :l lefts
        :r rights}
   \.  {}})

(defn pipe-neighbor [data x y]
  (let [[right left down up #_#_up down :as nebs]   (u/neighbors4 x y)
        ;;symbol connectedness is inverted.
        [rc lc dc uc #_#_uc dc :as cs] (mapv (fn [[x y]]
                                     (get-in data [y x])) nebs)
        c             (get-in data [y x]) ;;row major ugh!
        {:keys [u d l r]} (valid c)]
    (if (not (or u d l r))
      #{}
      (let [viables [(when (and r rc (r rc)) right)
                     (when (and l lc (l lc)) left)
                     (when (and u uc (u uc)) up)
                     (when (and d dc (d dc)) down)]]
        ;;now merge the connectedness together.
        (->> viables
             (filter identity)
             set)))))

(defn parse-input [txt]
  (let [data (->> txt
                   s/split-lines
                   (mapv vec))
        grid (->> (u/adjacency (count (first data)) (count data)
                               :neighbors-fn #(pipe-neighbor data %1 %2))
                  (reduce-kv (fn [acc k {:keys [coord] :as node}]
                               (let [[x y] coord] ;;row major!
                                 (assoc acc k (assoc node :data (get-in data [y x])))))
                             {}))]
    {:data data
     :w (count (first data))
     :h (count data)
     :init (->> grid (some (fn [[k {:keys [data]}]]
                             (when (= data \S)
                               k))))
     :grid grid}))

(defn depth-walk [{:keys [grid init]}]
  (let [init-state {:from init
                    :spt  {init init}
                    :dist {init 0}
                    :fringe (list [init 0])}]
    (loop
        [{:keys [spt dist fringe] :as state} init-state]
    (if-let [[current weight] (first fringe)]
      (if (and (not= current init) (dist current))
        (recur (update state :fringe pop))
        (let [nebs    (->> (get-in grid [current :neighbors])
                           (filter (fn [n] (not (dist n))))
                           (map (fn [nd] [nd (inc weight)])))]
          (recur {:fringe (into (pop fringe) nebs)
                  :spt    (reduce (fn [acc [nd _]]
                                    (assoc acc nd current)) spt nebs)
                  :dist  (assoc dist current weight)})))
        state))))

(defn get-path [{:keys [from spt dist]}]
  (let [[furthest d] (->> dist (sort-by (comp - val)) first)]
    (loop [path (list furthest)]
      (let [nxt (spt (peek path))]
        (if (= nxt (peek path))
          (vec  path)
          (recur (conj path nxt)))))))

(defn furthest [path]
  (let [steps  (subvec path 1)]
    (inc (quot (count steps) 2))))

(defn solve1 []
  (->> (u/slurp-resource "day10.txt")
       parse-input
       depth-walk
       get-path
       furthest))

;;part2
;;find the path.
;;order it clockwise.
;;define inside as being to the right.
;;walk the path, collecting inside adjacent nodes (both \. and "right")
;;flood fill from these nodes, where neighbors are dots.


(defn get-S [{:keys [grid init]} path]
  (let [[l start r] [(last path) init (second path)]
        [s1 s2 :as shapes] (mapv #(-> % grid :data) [l r])
        res   (case shapes
                [\J \7] \|
                [\| \|] \|
                [\- \-] \-
                [\L \J] \-
                [\- \J] \-
                [\F \|] \|
                (throw (ex-info "unknown pair" {:in [s1 s2]})))]
    (println [:replacing [s1 \S s2] :with [s1 res s2]])
    res))

(defn hscan [{:keys [w h grid data init] :as ctx} path]
  (let [knowns (->> path
                    (map (fn [nd] (-> nd grid :coord)))
                    set)
        vert #{\F \7 \|}
        new-S (get-S ctx path)
        [x y] (get-in grid [init :coord])
        data (assoc-in data [y x] new-S)]
      (->>  (for [y (range h)]
              (->> (range w)
                   (reduce (fn [{:keys [inside acc prev] :as res } x]
                             (let [c      (get-in data [y x])
                                   resnxt (assoc res :prev c)
                                   known?  (knowns [x y])]
                               (if known?
                                 (if  (not (vert c))
                                   resnxt
                                   (update resnxt :inside not))
                                 (if inside
                                   (update resnxt :acc conj [x y])
                                   resnxt))))
                           {:inside false :acc [] :prev nil})
                   :acc))
            (apply concat))))


(def isample
  "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(def sample2
".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(def bigsample
"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"
  )

(defn render [{:keys [grid w]}]
  (->> grid  (sort-by key) (partition w)
       (map (fn [xs]
              (s/join (map (comp :data val) xs))))
       (s/join \newline)
       println))


(defn render-path [{:keys [grid w h] :as ctx} p]
  (->> p
       (reduce (fn [acc k]
                 (assoc-in acc [k :data] \* ))
               grid)
       (assoc ctx :grid)
       render))

(defn solve2 [txt]
  (let [{:keys [grid] :as ctx}
          (->> txt
               parse-input)
        path  (-> ctx
                  depth-walk
                  get-path)]
    (->> (hscan ctx path)
         count)))
