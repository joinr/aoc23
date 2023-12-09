(ns aoc23.day9
  (:require [aoc23.util :as u]
            [clojure.string :as s]))

(def sample
"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map u/read-as-vector)))


(defn vtrim [xs]
  (let [n (count xs)]
    (when (>= n 4)
      (subvec xs 1 (dec n)))))

(defn differences [xs]
  (when (> (count xs) 1)
    (mapv (fn [i]
            (- (xs i) (xs (dec i))))
          (range 1 (count xs)))))

(defn final [xs]
  (let [n (count xs)
        l (- (xs 1) (xs 0))
        r (- (peek xs) (xs (- n 2)))]
    [l r]))


(defn triangle [xs]
  (->> xs
       (iterate differences)
       (take-while #(not= (% 0) (peek %) 0))))

(defn walk-back [xs]
  (->> xs
       reverse
       (reduce (fn [acc xs]
                 (+ acc (peek xs))) 0)))



(defn next-value [xs]
  (->> xs triangle walk-back))

(defn solve1 []
  (->> (u/slurp-resource "day9.txt")
       parse-input
       (map next-value)
       (reduce +)))

(defn walk-front [xs]
  (let [rxs  (->> xs reverse)
        init (ffirst rxs)]
    (->> rxs
         rest
         (reduce (fn [acc xs]
                   (- (xs 0) acc)) init))))

(defn prior-value [xs] (-> xs triangle walk-front))

(defn solve2 []
  (->> (u/slurp-resource "day9.txt")
       parse-input
       (map prior-value)
       (reduce +)))
