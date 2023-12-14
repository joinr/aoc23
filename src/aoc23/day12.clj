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
        chunks (u/read-as-vector r)
        length (count l)
        hashes  (->> chunks (reduce +))
        pads    (dec (count chunks))
        used    (+ hashes pads)]
    {:original l
     :chunks chunks
     :length length
     :dots   (- length hashes)}))

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map parse-line)))

;;walk the string, looking to place the next run.
;;to place a run, have to have space for it.
;;if there's already a character there, all chars must match.

;;if we go char by char,
(defn char-at [^CharSequence s ^long n]
  (.charAt s n))


(defn possible? [^String original  ^long idx ^long pounds ^long dots ]
  (let [bound (count original)]
    (if (> (+ pounds dots) bound)
      false
      (loop [idx idx
             dots dots
             pounds pounds]
        (cond (= idx bound)
              (and (zero? pounds) (zero? dots))
              (pos? pounds)
              (let [c (char-at original idx)]
                (if (or (= c \#) (= c \?))
                  (recur (unchecked-inc idx)
                         dots
                         (unchecked-dec pounds))
                  false))
              (pos? dots)
              (let [c (char-at original idx)]
                (if (or (= c \.) (= c \?))
                  (recur (unchecked-inc idx)
                         (unchecked-dec dots)
                         pounds)
                  false))
              :else          true)))))

(defn gen
  [s dots chunks idx]
   (if (= s "")
     (if (or (neg? dots) (seq chunks))
       0
       1)
     (case (char-at s 0)
       \. (if (pos? dots)
            (gen (subs s 1) (dec dots) chunks (inc idx))
            0)
       \# (if-let [n (nth chunks 0 nil)]
            (let [added-dot (if (= (count chunks) 1) 0 1)
                  size      (+ n added-dot)]
              (if (possible? s 0 n added-dot)
                (gen (subs s size) (- dots added-dot) (subvec chunks 1)
                     (+ idx size))
                0))
            0)
       \? (let [l (gen (str \. (subs s 1)) dots chunks idx)
                r (gen (str \# (subs s 1)) dots chunks idx)]
            (+ l r)))))

(defn gen-cached
  ([cache s dots chunks idx]
   (let [store (fn [k v]
                 (.put ^java.util.Map cache k v)
                 v)
         aux (fn aux [s dots chunks idx]
               (let [k [s dots chunks idx]]
                 (or (get cache k)
                   (store k
                          (if (= s "")
                            (if (or (neg? dots) (seq chunks))
                              0
                              1)
                            (case (char-at s 0)
                              \. (if (pos? dots)
                                   (aux (subs s 1) (dec dots) chunks (inc idx))
                                   0)
                              \# (if-let [n (nth chunks 0 nil)]
                                   (let [added-dot (if (= (count chunks) 1) 0 1)
                                         size      (+ n added-dot)]
                                     (if (possible? s 0 n added-dot)
                                       (aux (subs s size) (- dots added-dot) (subvec chunks 1)
                                            (+ idx size))
                                       0))
                                   0)
                              \? (let [l (aux (str \. (subs s 1)) dots chunks idx)
                                       r (aux (str \# (subs s 1)) dots chunks idx)]
                                   (+ l r))))))))]
     (aux s dots chunks idx)))
  ([s dots chunks idx] (gen-cached (java.util.HashMap.) s dots chunks idx)))

(defn solve [{:keys [original chunks length dots] :as in}]
  (gen original dots chunks 0))
(defn solve-cached [{:keys [original chunks length dots] :as in}]
  (gen-cached original dots chunks 0))

(defn solve1 [txt]
  (->> txt
       s/split-lines
       (map parse-line)
       (map solve-cached)
       (reduce +)))

#_(solve1 (u/slurp-resource "day12.txt"))

;;part2

(defn unfold [ln n]
  (let [[l r] (s/split ln #" ")
        l (s/join "?" (repeat n l))
        r (s/join "," (repeat n r))]
    (str l " " r)))

(defn solve2 [txt]
   (->> txt
        s/split-lines
        (map #(unfold % 5))
        (map parse-line)
        (map solve-cached)
        (reduce +)))

#_(solve2 (u/slurp-resource "day12.txt"))
