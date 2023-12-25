(ns aoc23.day17
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample
"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(def optimal
"2003400001323
3200003505623
3255245600054
3446585845052
4546657867006
1438598798404
4457876987706
3637877979600
4654967986880
4564679986450
1224686865500
2546548887705
4322674655500")

(defn parse-input [txt]
  (let [entries (->> txt
                     s/split-lines
                     (mapv (fn [x] (-> (s/join "," x) u/read-as-vector))))]
    {:h (count entries)
     :w (count (first entries))
     :entries entries}))

(def dirs
  {:n [0 -1]
   :e [1  0]
   :s [0  1]
   :w [-1 0]})

(def rights {:n :e, :e :s, :s :w, :w :n})
(def lefts {:n :w, :e :n, :s :e, :w :s})

(defn compass-shift
  ([dir coord size]
   (let [[dx dy]  (dirs dir)
         dx (* dx size)
         dy (* dy size)]
     [(+ (coord 0) dx)
      (+ (coord 1) dy)]))
  ([dir coord] (compass-shift dir coord 1)))

(def gas-max 3)

;;turning combines facing + moving forward. includes cost of first
;;move in gas calcs.
(defn turn
  ([heading {:keys [dir gas coord]} size]
   (let [new-dir ((case heading :l lefts :r rights) dir)]
     {:dir new-dir :gas (dec gas-max) :coord (compass-shift new-dir coord size)}))
  ([heading state] (turn heading state 1)))

(defn forward
  ([{:keys [dir gas coord] :as state} size]
   {:dir dir :gas (- gas size) :coord (compass-shift dir coord size)})
  ([state] (forward state 1)))

;;state will be direction, gas, coord.
;;if we run out of gas, we have to turn 90.
;;can't go backwards.
(defn neighbors [{:keys [dir gas coord] :as state}]
  (let [l (turn :l state)
        r (turn :r state)] ;;can always turn left or right.
    ;;if we have gas, we can go forward up to gas times, reducing gas.
    (case gas
      0  [l r]
      [l r (forward state)])))

(defn clamped [w h {:keys [coord] :as state}]
  (let [[x y] coord]
    (when (and (<  -1 x  w)
               (<  -1 y  h))
      state)))

;;returns a pair of [state weight] for each possible
;;neighbor, for every possibl neighbor.
(defn weighted-neighbors [{:keys [w h entries] :as ctx} state]
  (->> state
       neighbors
       (filter #(clamped w h %))
       (map (fn [{:keys [coord] :as state}]
              (let [[x y] coord]
                [state (get-in entries [y x])])))))

;;starting state would have 3 gas. direction not as important, but south makes sense,
;;leaves only forward/left as viable moves.
(def start-state {:dir :s :gas 3 :coord [0 0]})

(defn end? [w h state]
  (= (state :coord) [w h]))

(defn first-path [res]
  (let [[from to] (res :found-path)]
    (let [xs (first (u/paths (res :spt) from to))]
      [((res :dist) to) xs])))

(defn all-paths [res]
  (let [[from to] (res :found-path)]
    (->> (u/paths (res :spt) from to)
         #_(map render-path))))


;;need to add a fake node to the end.
;;the only neighbor of [xmax ymax] is the :end node.
(defn dj-search [{:keys [w h] :as ctx}  from
                    & {:keys [make-fringe weightfn]
                       :or {make-fringe (partial u/min-pq-by hash)
                            on-visit identity
                            weightfn (fn [source]
                                       (weighted-neighbors ctx source))}}]
  (let [init {:spt  {from from}
              :dist {from 0}
              :fringe (make-fringe [0 from])}
        xmax (dec w)
        ymax (dec h)]
    (loop [{:keys [spt dist fringe v] :as state} init]
      (if-let [source (u/peek-fringe fringe)]
        (if (= source :end)
          (assoc state :found-path [from source])
          (let [nebs (if (end? xmax ymax source)
                       [[:end 0]]
                       (weightfn source))
                next-state (reduce  (fn [acc [sink w]]
                                        (u/relax acc source sink w))
                                      (update state :fringe u/pop-fringe)
                                      nebs)]
            (recur next-state)))
        state))))

(defn solve1 [txt]
  (-> txt
      parse-input
      (dj-search start-state)
      first-path
      first))

;;part2
;;ultra crucible gas is 10 now.
;;moves cost 4 minium, up to 10.
;;so l,r,forward neighbors are now 4 moves away.if we have gas <=6, then we can
;;move forward by 1's.


(def sample2
"111111111111
999999999991
999999999991
999999999991
999999999991")

(defn ultra-neighbors [{:keys [dir gas coord] :as state}]
  (let [l (-> (turn :l state 4) (assoc :gas 6))
        r (-> (turn :r state 4) (assoc :gas 6))] ;;can always try to turn left or right.
    ;;if we have gas, we can go forward up to gas times, reducing gas.
    ;;for ultras, on the first forward move, we have to move 4.
    ;;after that it can be incremental.
    (case gas
      0  [l r]
      10 [l r (forward state 4)]
      [l r (forward state 1)])))

(defn between [[x1 y1] [x2 y2]]
  (if (= x1 x2) ;;same col
     (if (< y1 y2) ;;up
       (for [i (range 1 (- y2 y1))]
         [x1 (+ y1 i)])
       (for [i (range 1 (- y1 y2))]
         [x1 (- y1 i)]))
     ;;same row
     (if (< x1 x2)
       ;;right
       (for [i (range 1 (- x2 x1))]
         [(+ x1 i) y1])
       (for [i (range 1 (- x1 x2))]
         [(- x1 i) y1]))))

(defn weight [entries [x y]]
  (get-in entries [y x]))

;;need to tally the cost of traversing multiple neighbors now.
(defn weighted-ultra-neighbors [{:keys [w h entries] :as ctx}
                                {:keys [coord] :as state}]
  (let [[x y] coord]
    (->> state
         ultra-neighbors
         (filter #(clamped w h %))
         (map (fn [target]
                (let [coord2   (target :coord)
                      w        (weight entries coord2)
                      interims (between coord coord2)
                      ws       (->> interims (map #(weight entries %)) (reduce +))]
                    [target (+ w ws)]))))))


(def ultra-start (assoc start-state :gas 10))
(defn ultra-search [ctx  from]
  (dj-search ctx from :weightfn (fn [source] (weighted-ultra-neighbors ctx source))))


(defn solve2 [txt]
  (-> txt
      parse-input
      (ultra-search ultra-start)
      first-path
      first))
