(ns aoc23.day12
  (:require [aoc23.util :as u]
            [clojure.string :as s]))

(def sample
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

;;I think we can just slide the intervals around and generate
;;strings then compare them with the base.


(defn parse-line [ln]
  (let [[l r]  (s/split ln #" ")
        odds   (u/read-as-vector r)
        length (count l)
        hashes  (->> odds (reduce +))
        pads    (dec (count odds))
        used    (+ hashes pads)
        slack   (- length used)]
    {:original l
     :odds odds
     :length length
     :slack slack
     :bound (inc (count odds))}))

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map parse-line)))

;;"###.##.#...."
;;{:length 12
;; :evens [0 1 1 4]
;; :odds  [3 2 1]
;; :slack 4}

;;so 1 1 3 ->
;; "#" "#" "###"

;;walk the string, looking to place the next run.
;;to place a run, have to have space for it.
;;if there's already a character there, all chars must match.

(defn pieces [xs]
  (for [n xs]
    (apply str (repeat n \#))))

;;solution may look like
;;[piece1 idx
;; piece2 idx
;; piece3 idx]

;;if we go char by char,
(defn char-at [^CharSequence s ^long n]
  (.charAt s n))


;;for testing
(defn dots [n]
  (apply str (repeat  n \.)))

(defn pounds [n]
  (apply str (repeat  n \#)))

(defn viable? [^String original  ^long idx ^long dots ^long pounds]
  (let [bound (count original)]
    (loop [idx idx
           dots dots
           pounds pounds]
      (cond (pos? dots)
              (let [c (char-at original idx)]
                (if (or (= c \.) (= c \?))
                  (recur (unchecked-inc idx)
                         (unchecked-dec dots)
                         pounds)
                  false))
            (pos? pounds)
              (let [c (char-at original idx)]
                (if (or (= c \#) (= c \?))
                  (recur (unchecked-inc idx)
                         dots
                         (unchecked-dec pounds))
                  false))
              :else          true))))

(defn gen
  ([{:keys [length odds original] :as state }  bound evens slack n current]
   (if (= n bound) ;;done
     [current]
     (let [internal? (< 0 n (dec bound))
           lb (cond (= n (dec bound))  slack ;;have to distribute remainder.
                    internal? 1 ;;internal, have to distribute 1.
                    :else               0)]
       (->> (for [i (range lb (+ slack (if internal? 2 1)))] ;;internal gets 1 for free.
              (let [enxt (conj evens i)
                    snxt (if internal?
                           (- slack (dec i)) ;;padded already added.
                           (- slack i))
                    viable  (viable? original (count current) i (nth odds n 0))]
                (when viable
                  (let [dots       (dots i)
                        new-string (if (< n (dec bound))
                                     (str current dots (pounds (nth odds n)))
                                     (str current dots))]
                    (gen state bound enxt snxt (inc n) new-string)))))
            (apply concat)))))
  ([{:keys [length odds original slack bound] :as state}]
   (gen state bound [] slack 0 "")))

#_
(def res (vec (gen {:length 12 :odds [3 2 1] :original "?###????????"} 4 [] 4 0 "")))


(defn solve1 [txt]
  (->> txt
       parse-input
       (map gen)
       (map count)
       (reduce +)))

;;part2

;;naive unfolding...
;;probably fails.

(defn unfold [ln n]
  (let [[l r] (s/split ln #" ")
        l (s/join "?" (repeat n l))
        r (s/join "," (repeat n r))]
    (str l " " r)))

(defn solve2 [ n txt]
  (->> txt
       s/split-lines
       (map #(unfold % n))
       (map parse-line)
       (map gen)
       (map count)
       (reduce +)))

(defn compare [n txt]
  (->> txt
       s/split-lines
       (map #(unfold % n))
       (map parse-line)
       (map gen)
       (map count)
       (reduce +)))

(defn folds [n ln]
  (for [i (range 1 (inc n))]
    (parse-line (unfold ln i))))
