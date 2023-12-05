(ns aoc23.day5
  (:require [aoc23.util :as u]
            [clojure.string :as s]))

(def sample
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")


;;looks like memoized interval maps are called for based on input.

;;they messed with the wording to make it harder to parse.
;;humans would expect a map of seed-to-soil to have
;;seed soil n, but they encode it in reverse.
;;soil seed n. we will coerce it into something more useful
;;so we get a map of src->des.
;;so our descriptive names actually match the mapping now.
;;we store the mapping as [from-left from-right offset]
(defn clarify-interval [[dest src n]]
  (let [bnd (dec n)]
    [src (+ src bnd) (- dest src)]))

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (partition-by #{""})
       (keep #(when (-> % first (not= "")) %))
       (map (fn [xs] (s/join " " xs)))
       (map (fn [ln] (-> ln
                         (s/replace #" map" "")
                         (s/split #":"))))
       (map (fn [[l r]]  [(keyword l) (as-> (u/read-as-vector r) res
                                        (if (not= l "seeds")
                                          (->> (partition 3 res)
                                               (mapv clarify-interval)
                                               (sort-by first)
                                               vec)
                                          res))]))
       (into {})))

;;we can look at this like a prism using intervals to derive
;;a mapped value.
;;when we get a value from an interval map, we check to
;;see if it intersects any intervals first.
;;we can store the intervals in a sorted set too and
;;use range queries for moar efficiency.
;;given the problem size, just scanning for collisions is
;;simple enough.

(defn between [l r n]
  (and (>= n l)
       (<= n r)))

(defn interval-get [xs k]
  (reduce (fn [acc [l r off]]
            (if (between l r k)
              (reduced (+ k off))
              acc))
          k xs))

(def loc-path [:seed-to-soil
               :soil-to-fertilizer
               :fertilizer-to-water
               :water-to-light
               :light-to-temperature
               :temperature-to-humidity
               :humidity-to-location])

(defn get-path [ctx k path]
  (if-let [p (first path)]
    (let [nxt (interval-get (ctx p) k)]
      (recur ctx nxt (rest path)))
    k))

(defn resolve-seeds [ctx]
  (->> ctx
       :seeds
       (map #(get-path ctx % loc-path))))

(defn solve1 []
  (->> (u/slurp-resource "day5.txt")
       parse-input
       resolve-seeds
       (reduce min)))

;;part 2

;;we can't iterate. we can push an interval down,
;;looking at the impact on the interval(s).
;;so compute an interval map as we go.
;;cases -> no collisions, no changes.
;;if any interval collides with another
;;-> interval is contained, collider
;;   gets a new offset.
;;   interval is partially contained
;;      - split into two new intervals.
;;      - sub interval is contained and inherits
;;        new offset.

;;           so [79 92]
;;:seed-to-soil collides [50 97 2], contained.
;;mapping range [79 92] into seed->soil uses offset 2.
;;becomes [81 93]
;;:soil-to-fertilizer no collision.
;;still [81 93]
;;fertilizer-to-water no collision
;;water-to-light [25 94 -7] collides.
;;contained.  [81-7 93-7]
;;[74 86]

;;always sort by left coordinate.
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
          (when (and (<= rx1 lx1) (>= rx2 lx2)) l) ;;[    -----    ]
          )))))

;;if we return the interval as its own intersection, that means it's contained.
;;if we return a diffrent interval, we have to split the original interval
;;into two new intervals.

(defn inc-by [n xs]
  (mapv #(+ % n) xs))

(defn split-interval [off l r ]
  (if-let [res (intersection l r)]
    ;;split or not.
    (if (= res l)
      ;;contained. apply offset
      [:contained l]
      ;;need to split.
      (let [[l1 r1] l
            [l2 r2] res]
        (if (< l1 l2)
        [:split-right [[l1 (dec r1)]    res]]
        [:split-left  [res [(inc r2) r1]]])))
    ;;otherwise leave it alone.
    l))


;;need to define an interval fold.
;;we take an initial interval:

(def tst [[79 92]]) ;;initial seed interval(s).

(def tstmap   [[50 97 2] [98 99 -48]])

;;naive way is to reduce over all intervals.  we could do this
;;in order and efficiently step through, since everything is sorted.

;;we either replace an older seed interval with a new result or leave it alone.
;;if the new result is a split, then the two new split intervals swap in.
;;so we can accumulate an interval vector this way without issue.
;;we could do an interval tree for better speed (broad phase collisions),
;;but this is fine.
(defn advance [seed-intervals map-intervals]
  (u/log [:init seed-intervals :map map-intervals])
  (loop [seeds      seed-intervals
         remaining  map-intervals]
    (if-let [nxt (first remaining)]
      (let [[l r off] nxt
            new-seeds (reduce (fn [acc seed-interval]
                                (let [res (split-interval off seed-interval [l r])
                                      k   (first res)]
                                  (u/log [:result res off])
                                  (if (identical? res seed-interval)
                                       (conj acc seed-interval) ;;no change, keep testing.
                                    (case k
                                      :contained  (conj acc (with-meta (second res) {:offset off} ))
                                      (let [[lnew rnew] (second res)]
                                        (case k
                                          :split-left  (conj acc (with-meta lnew {:offset off} ) rnew)
                                          :split-right (conj acc lnew (with-meta rnew {:offset off} ))))))))                              [] seeds)]
        (recur new-seeds (rest remaining)))
      (->> seeds
           (map (fn [x]
                   (if-let [off (-> x meta :offset)]
                     (inc-by off x)
                     x)))
           (sort-by first)
           vec))))

(defn project-seed-interval [ctx path seed-intervals]
  (->> (map #(get ctx %) path)
       (reduce advance seed-intervals)))

(defn minimum-locs [{:keys [seeds] :as ctx}]
  (let [seed-intervals (for [xy (map (fn [[l r]]
                                       [l (+ l (dec r))]) (partition 2 seeds))]
                         [xy])]
    (->> (for [ival seed-intervals]
           [ival (project-seed-interval ctx loc-path ival)])
         (mapcat second)
         (map first)
         (reduce min))))

(defn solve2 []
  (->> (u/slurp-resource "day5.txt")
       parse-input
       minimum-locs))
