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
                               (assoc acc k (assoc node :data (get-in data coord))))
                             {}))]
    {:data data
     :init (->> grid (some (fn [[k {:keys [data]}]]
                             (when (= data \S)
                               k)))) :grid grid}))

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


(defn inside-dir [[x1 y2]]
  )
(defn clockwise-path [{:keys [data grid] :as ctx}]
  (let [path  (->> ctx
                   depth-walk
                   get-path)
        init (first path)
        coord (-> init grid :coord)
        origin [(- )]]
    {:coord coord
     :path  path}))
