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
  (->> (s/split txt #"\r\n\r\n|\n\n")
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
(def sample2 (second samples))

(defn freqs [xs]
  (mapv frequencies xs))

(defn adjacents [fs]
  (->> (count fs)
       dec
       (range )
       (filter (fn [n]  (= (fs n) (fs (inc n)))))))

(defn scan-reflection [xs fs n]
  (let [bound (count fs)
        l (dec n)
        r (+ n 2)
        span (inc (min l (- (dec bound) r)))]
    (when (= (xs n) (xs (inc n)))
      (->> (concat
            (for [i (range 0 span)]
              (let [lidx (- l i)
                    ridx  (+ r i)]
                #_(println (fs lidx) (fs ridx))
                (or (= (fs lidx) (fs ridx))
                    (do ; (println [:not=freq lidx ridx (fs lidx) (fs ridx)])
                      false))))
            (for [i (range 0 span)]
              (let [lidx (- l i)
                    ridx  (+ r i)]
                #_(println (cs lidx) (cs ridx))
                (or  (= (xs lidx) (xs ridx))
                     (do ; (println [:not= lidx ridx (cs lidx) (cs ridx)])
                       false)))))
           (every? identity)))))

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


(defn summarize [xs]
  (try (let [v (v-reflection xs)
             h (when-not v (h-reflection xs))]
         (+ (if v (inc v) 0)
            (if h (* 100 (inc h)) 0)))
       (catch Exception e (throw (ex-info "bad input" {:in xs})))))

(defn solve1 [txt]
  (->> txt
       parse-input
       (map summarize)
       (reduce +)))

#_
(solve1 (u/slurp-resource "day13.txt"))
