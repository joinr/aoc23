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
                (or (= (fs lidx) (fs ridx))
                    (do ; (println [:not=freq lidx ridx (fs lidx) (fs ridx)])
                      false))))
            (for [i (range 0 span)]
              (let [lidx (- l i)
                    ridx  (+ r i)]
                (or  (= (xs lidx) (xs ridx))
                     (do ; (println [:not= lidx ridx (cs lidx) (cs ridx)])
                       false)))))
           (every? identity)))))


(defn reflection [fs xs adjs]
  (->> adjs
       (some (fn [n]
               (when (scan-reflection xs fs n) n)))))

(defn scan-reflections [xs]
  (let [fs (freqs xs)]
    (->> fs
         adjacents
         (reflection fs xs))))

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

;;yields the [idx entry]
;;to change r into l by 1 if possible.
;;returns :none if no changes, nil if infeasible.
(defn smudge [l r]
  (reduce-kv (fn [acc idx k]
               (cond (= (r idx) k) acc
                     (vector? acc)      (reduced nil) ;;already smudged.
                     :else    [idx k])) :none l))

;;if the char freqs are equal, or off by one.
;;if smudge adjacent, [n] implies equal, or where is
;;the character difference [n idx char]

(defn gross-misses[l r]
  (abs (- (l \#) (r \#))))

(defn smudge-adjacent [known fs xs]
  (->> (count fs)
       dec
       range
       (map (fn [n]
              (when (not (known n))
                (let [l (fs n)
                      r (fs (inc n))
                      lv (xs n)
                      rv (xs (inc n))
                      offs (gross-misses l r)]
                  (case offs
                    0  (when  (= lv rv)
                         [n]) ;;infeasible otherwise.
                    1  (when-let [res (smudge lv rv)]
                         (case res
                           :none  (throw (ex-info "shouldn't happen!" {:in [lv rv]}))
                           (into [n] res)))
                    nil)))))
       (filter identity)))

;;a candidate is an adjacent pair that is only 1 off.
(defn smudge-candidates [fs xs]
  (let [adjs      (adjacents xs)
        known     (if-let [r (reflection fs xs adjs)]
                    #{r}
                    #{})]
    (smudge-adjacent known fs xs)))

(defn scan-smudge [xs fs n smudge-count]
  (let [bound (count fs)
        l (dec n)
        r (+ n 2)
        span (inc (min l (- (dec bound) r)))
        smudges (atom smudge-count) ;;pre-smudged or not.
        smudge= (fn [l r]
                  (when-let [res  (smudge l r)]
                    (or (= res :none)
                        (>= (swap! smudges dec) 0))))]
    (->> (concat
          (for [i (range 0 span)]
            (let [lidx (- l i)
                  ridx  (+ r i)
                  offs  (gross-misses (fs lidx) (fs ridx))]
              (or (zero? offs)
                  (and (= offs 1) (> @smudges 0)))))
          (for [i (range 0 span)]
            (let [lidx (- l i)
                  ridx  (+ r i)
                  res  (smudge= (xs lidx) (xs ridx))]
              res
                  )))
         (every? identity))))

(defn scan-smudges [xs]
  (let [fs (freqs xs)]
    (->> (smudge-candidates fs xs)
         (some (fn [[n idx _]]
                  (when (scan-smudge xs fs n (if idx 0 1))
                    n))))))


(defn v-smudge [xs]
  (scan-smudges (col-major xs)))
(defn h-smudge [xs]
  (scan-smudges  xs))


(defn summarize-smudge [xs]
  (try (let [v (v-smudge xs)
             h (h-smudge xs)]
         (+ (if v (inc v) 0)
            (if h (* 100 (inc h)) 0)))
       (catch Exception e (throw (ex-info "bad input" {:in xs})))))


(defn solve2 [txt]
  (->> txt
       parse-input
       (map summarize-smudge)
       (reduce +)))
