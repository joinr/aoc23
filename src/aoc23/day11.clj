(ns aoc23.day11
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample
"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....." )

;;need to expand graph (maybe can handle it by weights but meh).

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (mapv vec)))


;;bug!
(defn scan-galaxies [rows]
  (->> (for [[y r] (map-indexed vector rows)
             [x e] (map-indexed vector r)]
         (when (= e \#)
               [x y]))
       (filter identity)
       (sort-by (fn [[x y]] [y x]))
       (map-indexed (fn [idx xy] [(inc idx) xy]))
       (into {})))

(defn singletons [gals]
  {:xknown (->> gals (map #(-> % second first)) set)
   :yknown (->> gals (map #(-> % second second)) set)})

;;naive.
(defn expand [rows]
  (let [{:keys [xknown yknown]} (-> rows scan-galaxies singletons)]
    (->> (for [y (range (count rows))]
           (let [r (->> (rows y)
                        (map-indexed vector)
                        (reduce (fn [acc [x e]]
                                  (if (xknown x)
                                    (conj acc e)
                                    (conj acc e e))) []))]
             (if (yknown y)
               [r]
               [r r])))
         (apply concat)
         vec)))

(defn galaxy-pairs [gals]
  (let [gnames (-> gals keys vec)
        n (count gnames)]
    (for [i (range n)
          j (range i n)
          :when (not= i j)]
      [(gnames i) (gnames j) ])))

;;instead of searching, just do manhattan.
(defn solve1 [txt]
  (let [rows (->> txt
                  parse-input
                  expand)
        galaxies (scan-galaxies rows)]
    (->>  (for [[l r] (galaxy-pairs galaxies)]
            (u/manhattan-xy (galaxies l) (galaxies r)))
          (reduce +))))

;;better expansion algo
;;we retain the row/columns that expand or dilate, and
;;by some amount.
;;from there we can determine if the line cross which rows/cols.
;;compute the manhattan distance as before.
;;add up the row/col to the distance.

(defn expand2 [rows]
  (let [gs (-> rows scan-galaxies)
        {:keys [xknown yknown]}  (singletons gs)
        w (count (first rows))
        h (count rows)]
    {:rows (->> (range w) (filter (complement yknown)) vec)
     :cols (->> (range h) (filter (complement xknown)) vec)
     :galaxies gs }))


(defn values-between
  ([l r xs]
   (if (<= l r)
     (into [] (comp (drop-while #(< % l)) (take-while #(<= % r))) xs)
     (recur r l xs))))

;;should be able to do manhattan + number of erows crossed + number of ecols crossed
(defn crossings [{:keys [rows cols galaxies] :as ctx} from to]
  (let [[x1 y1] (galaxies from)
        [x2 y2] (galaxies to)]
     [(values-between y1 y2 rows)
      (values-between x1 x2 cols)]))

(defn expanded-distance
  ([{:keys [galaxies] :as ctx} cost from to]
   (let [base-distance    (u/manhattan-xy (galaxies from) (galaxies to))
         crossed         (crossings ctx from to)
         total-crossings (->> (map count crossed)
                              (reduce +))
         added           (* cost total-crossings)]
     (+ base-distance added (- total-crossings))))
  ([ctx from to] (expanded-distance ctx 1 from to)))

(defn solve2 [txt cost]
  (let [ctx (->> txt
                 parse-input
                 expand2)]
    (->>  (for [[l r] (galaxy-pairs (ctx :galaxies))]
            (expanded-distance ctx cost l r))
          (reduce +))))
#_
(solve2 (u/slurp-resource "day11.txt") 1000000)

;;legacy naive ssp solution for posterity.

;; (defn init-graph [xs]
;;   (let [h (count xs) w (count (first xs))]
;;     (u/entries->graph  (mapv (fn [_] 1) (apply concat xs)) w h)))

;; (defn path-state [rows]
;;   (let [gs (scan-galaxies rows)
;;         gr (init-graph rows)]
;;     {:w (count (first rows))
;;      :h (count (second rows))
;;      :galaxies gs
;;      :graph gr}))

;; 
;; (defn galaxy->nd [{:keys [w h galaxies]} g]
;;   (let [[x y]  (galaxies g)]
;;     (u/xy->idx w h x y)))
;;
;; (defn ssp-galaxy [{:keys [graph] :as ctx} from to]
;;   (let [res (u/best-first graph (galaxy->nd ctx from)
;;                           (galaxy->nd ctx to))]
;;     (u/recover-first-path res)))
