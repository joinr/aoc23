(ns aoc23.day21
  (:require [aoc23.util :as u]
            [aoc23.day9 :as nine]
            [clojure.string :as s]))


(def sample
"...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

;;I think we can break this down to
;;maintaining even and odd steps.
;;So 2 fringes.  When we want to find
;;new neighbors, we use the complement
;;of the current fringe to preclude
;;adding old neighbors.

;;So start with odd, #{S} wherever
;;S is.
;;Push S onto the fringe.
;;Compute neighbors, except
;;we don't revisit old neighbors.

;;Sort of like a BFS, except we want to
;;keep track of odd/even steps.
;;On the 1st step, we pop all neighbors
;;of the start.  They were found at
;;a distance of 2, but we have the possibility
;;of revisiting them by walking around randomly.
;;To prevent exhaustive pathing, we keep track
;of them as evens.

;;At the end, we need to compute
;;coverage.  We should be able to reach any even node.  As long
;;we limit our maximum depth to the step length,
;;we can reach any node < the step length.

(defn parse-input [txt]
  (let [rows (->> txt
                  s/split-lines
                  (mapv vec))
        w (count (first rows))
        h (count rows)
        start (first (for [y (range h)
                           x (range w)
                           :when (= (get-in rows [y x]) \S)]
                       [x y]))]
    {:w w
     :h h
     :start start
     :entries rows}))

(defn pos [x]
  (if (neg? x) (- x) x))

(defn tiled-get [entries w h  x y ]
  (let [x (pos (mod x w))
        y (pos (mod y h))]
    (get-in entries [y x])))

(defn g-neighbors [{:keys [w h entries] :as ctx} [x y]]
  (let [nebs (u/neighbors4 x y)]
    (->> nebs
         (filterv (fn [[x y]]
                    (and (<= 0 x w)
                         (<= 0 y h)
                         (not= (get-in entries [y x]) \#)))))))

(defn tiled-neighbors [{:keys [w h entries] :as ctx} [x y]]
  (let [nebs (u/neighbors4 x y)]
    (->> nebs
         (filterv (fn [[x y]]
                    (not= (tiled-get entries w h x y)  \#))))))

(defn breadth-walk [ctx start steps & {:keys [nebfn] :or {nebfn g-neighbors}}]
  (loop [{:keys [fringe dist spt] :as state}
         {:fringe (u/push-fringe (u/q) 0 start)
          :dist  {start 0}
          :spt   {start start}}]
    (if-let [source (u/peek-fringe fringe)]
      (if (>= (dist source) steps)
        (recur (update state :fringe u/pop-fringe)) ;;no neighbors.
        (let [neighbors  (->> (nebfn ctx source)
                              (filter (fn [coord]
                                        (not (spt coord))))
                              (mapv (fn [coord] [coord 1])))
              next-state (reduce (fn [acc [sink w]]
                                      (u/relax acc source sink w))
                                    (update state :fringe u/pop-fringe)
                                    neighbors)]
          (recur next-state)))
      state)))

(defn reachable
  ([ctx steps nebfn]
   (let [res    (breadth-walk ctx (ctx :start) steps :nebfn nebfn)
         select (if (even? steps) even? odd?)]
     (->> res
          :dist
          (filter (fn [[k d]] (select d)))
          count)))
  ([ctx steps] (reachable ctx steps g-neighbors)))

(defn solve1 [txt]
  (-> txt
      parse-input
      (reachable 64)))


;;part2
;;this one kicked me in the balls.  Had to pick up the 100lb phone to get some ideas
;;from reddit for progress.
#_
(for [i (range 1 10)] [(* i 65) (reachable ins (* i 65) tiled-neighbors)])


;; aoc23.day21> (def runs (vec (for [i (range 0 5)] [(+ 65 (* i 131)) (reachable ins (+ 65 (* i 131)) tiled-neighbors)])))

;; aoc23.day21> runs
;; [[65 3885] [196 34700] [327 96215] [458 188430] [589 311345]]
;; aoc23.day21> (nine/differences (mapv second runs))
;; [30815 61515 92215 122915]
;; aoc23.day21> (nine/differences (nine/differences (mapv second runs)))
;; [30700 30700 30700]

;;2nd order growth is the same.  should be able to compute
;;a closed form solution.  or reuse derivative stepper from before
;;(thanks reddit...).  We will just step through.s

(def target 26501365)
(defn step-width [{:keys [w h start]} steps]
  (let [init (start 0)]
    [init w (/ (- steps init) w)] ))

;;so we can compute the next number 202300 times...just keeping like 3
;;numbers.
(defn next-n
  ([xs ^long n]
   (if (zero? n)
     (last xs)
     (let [nxt (nine/next-value xs)
           dropped  (subvec xs 1)
           v     (-> (if (zero? (mod n 1000))
                       (into [] dropped) ;;prevent garbage leaks, maybe not necess..
                       dropped)
                     (conj  nxt))]
       (recur v (unchecked-dec n)))))
  ([xs]
   (next-n xs (- target (dec (count xs))))))

;;kick off the iterative process of computing the next
;;number based on derivatives.
(defn step-next [ctx samples]
  (let [[x0 x1 n] (step-width ctx target)]
    (next-n samples (- n (dec (count samples))))))

(defn solve2 [txt]
  (let [{:keys [w h start entries] :as ctx} (parse-input txt)
        offset (start 0)
        runs (vec (for [i (range 0 5)]
                    (let [n (+ offset (* i w))] ;;in the common inputs:  (+ 65 (* i 131))
                      (reachable ins n tiled-neighbors))))]
    (step-next ins runs)))

