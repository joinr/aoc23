(ns aoc23.day13
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample
"#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn parse-input [txt]
  (->> (s/split txt #"\n\n")
       (map (fn [txt]
              (->> txt
                  s/split-lines
                  (mapv vec))))))

(defn col-major [xs]
  (let [rows (count xs)
        cols (count (first xs))]
    (vec (for [i (range cols)]
           (mapv #(nth % i) xs)))))

(def samples (parse-input sample))
(def sample1 (first samples))

(defn freqs [xs]
  (mapv frequencies xs))

(defn adjacents [fs]
  (->> (count fs)
       dec
       (range )
       (filter (fn [n]  (= (fs n) (fs (inc n)))))))

(defn scan-reflection [xs fs n]
  (let [bound (count fs)
        r (- bound n)
        span (min r n)
        _ (println span)]
    (->> (concat
                (for [i (range 1 span)]
                  (let [lidx (- n i)
                        ridx  (+ (inc n) i)]
                    (println (fs lidx) (fs ridx))
                    (or (= (fs lidx) (fs ridx))
                        (do ; (println [:not=freq lidx ridx (fs lidx) (fs ridx)])
                            false))))
                (for [i (range 1 span)]
                  (let [lidx (- n i)
                        ridx  (+ (inc n) i)]
                    (println (cs lidx) (cs ridx))
                    (or  (= (xs lidx) (xs ridx))
                         (do ; (println [:not= lidx ridx (cs lidx) (cs ridx)])
                             false)))))
         (every? identity))))

(defn scan-reflections [xs]
  (let [fs (freqs xs)]
    (->> fs
         adjacents
         (some (fn [n]
                 (when (scan-reflection xs fs n) n))))))

(defn v-reflection [xs]
  (scan-reflections (col-major xs)))
(defn h-reflection [xs]
  (scan-reflections  xs))
