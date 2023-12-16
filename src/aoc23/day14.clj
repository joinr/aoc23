(ns aoc23.day14
  (:require [aoc23.util :as u]
            [clojure.string :as s]))

(def sample
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(def rock->num  {\# 2 \O 1 \. 0})
(def num->rock (zipmap (vals rock->num) (keys rock->num)))

(defn parse-input [txt]
  (let [rows  (->> txt
                   s/split-lines
                   (mapv vec))
        w (count (first rows))
        h (count rows)]
    (u/new-grid w h (->> rows (apply concat) (map rock->num)))))

(defn render [^aoc23.util.grid g]
  (dotimes [y (.h g)]
    (dotimes [x  (.w g)]
      (print (-> g (u/get-entry x y) num->rock)))
    (print \newline)))

(defn copy! [g]
  (assoc g :entries (aclone (:entries g))))

(defn vshift! [^aoc23.util.grid g]
  (let [bound (.h g)]
    (dotimes [x (.w g)]
      (loop [y 0
             prev -1]
        (when (< y bound)
          (let [e (.getEntry g x y)]
            (case (int e)
              0 (recur (unchecked-inc y) prev) ;;empty
              1 (let [dest (unchecked-inc prev)]
                  (if (not= y dest)
                    (do #_(println [:shifting [x y e] :to [x dest e]])
                        (.setEntry g x dest 1) ;;rock.  move the current rock to the prev + 1.
                        (.setEntry g x y 0)
                        (recur (unchecked-inc y) dest))
                    (recur (unchecked-inc y) y)))
              2 (recur (unchecked-inc y) y) ;;wall
              (throw (ex-info "bad value" {:in e}))
              )))))
    g))

;;for testing.
(defn vshift [g] (vshift! (copy! g)))

(defn calc-load [^aoc23.util.grid g]
  (let [bound (.h g)
        ^longs
        acc   (long-array 1)
        _     (aset acc 0 0)]
    (dotimes [x (.w g)]
      (dotimes [y bound]
        (let [e (.getEntry g x y)]
          (when (= e 1)
            (aset acc 0 (+ (aget acc 0) (- bound y)))))))
    (aget acc 0)))


(defn solve1 [txt]
  (->> txt
       parse-input
       vshift!
       calc-load))


;;we can cache our results and use splitting to limit the permutations.
;;

;;so split a row / col into subsequences between #.
;;count the rocks (stores the state).
;;should be a cacheable state of num rocks -> result.

;;for any number of rocks, we can generate a packed sequence.
(defn packed-rocks [size n]
  (vec (concat (repeat (- size n) \.) (repeat n \O))))


(defn parse-input2 [txt]
  (->> txt
       s/split-lines))

;;inefficient.
(defn col-major-str [xs]
  (let [n    (count (xs 0))]
    (->> (reduce (fn [^objects cols ^String x]
                   (dotimes [i n]
                     (-> ^java.lang.StringBuilder
                         (aget cols i)
                         (.append (.charAt x i))))
                   cols)
                 (object-array (repeatedly n #(java.lang.StringBuilder.)))
                 xs)
          (mapv str))))

(defn pack ^String [rocks length  dir]
  (case dir
    (:south :east) (str (s/join (repeat (- length rocks) \.))
                        (s/join (repeat rocks \O)))
    (str (s/join (repeat rocks \O))
         (s/join (repeat (- length rocks) \.)))))
(alter-var-root #'pack u/memo-3)

(defn count-rocks [^String s]
  (let [bnd (count s)]
    (loop [idx 0
           acc 0]
      (if (= idx bnd) acc
          (recur (unchecked-inc idx)
                 (if (= (.charAt s idx) \O)
                   (unchecked-inc acc)
                   acc))))))
(alter-var-root #'count-rocks u/memo-1)

(defn compute-splits [row]
  (let [pieces (atom [])]
    (->> row
         (partition-by (fn [c] (= c \#)))
         (map (fn [xs] (if (= (first xs) \#)
                         {:wall (count xs)}
                         {:space (count xs)})))
         (reduce (fn [idx chunk]
                   (if (:space chunk)
                     (do (swap! pieces conj [idx (+ idx (:space chunk))])
                         (+ idx (:space chunk)))
                     (+ idx (:wall chunk))))
                 0))
    @pieces))
(alter-var-root #'compute-splits u/memo-1)

(defn splits [^String s pieces]
  (->> pieces
       (mapv (fn [[l r]]
              [l (subs s l r)]))))

#_
(defn unsplit [^String s pieces]
  (reduce (fn [acc [l substring]]
            (splice acc l substring)) s pieces))

(defn packed-sequence
  ([^String s split-points dir]
   (->> (splits s split-points)
        (reduce (fn [acc [l piece]]
                  (let [packed (pack (count-rocks piece) (count piece) dir)]
                    (u/splice acc l packed)))
                s)))
  ([s dir] (packed-sequence s (compute-splits s) dir)))
(alter-var-root #'packed-sequence u/memo-3)

(defn shift [rows dir]
  (case dir
    (:east :west) (->> rows (mapv (fn [col] (packed-sequence col (compute-splits col)
                                                             dir))))
    (->> rows col-major-str (mapv (fn [col] (packed-sequence col (compute-splits col)
                                                             dir)))
         col-major-str)))

(defn wash-cycle [xs]
  (-> xs
      (shift :north)
      (shift :west)
      (shift :south)
      (shift :east)))

(defn col-load [col]
  (->> col
       (map vector (range (count col) 0 -1))
       (keep (fn [[n c]]
               (when (= c \O) n)))
       (reduce +)))

(defn north-load [rows]
  (->> rows
       col-major-str
       (map col-load)
       (reduce +)))

;;does our cycle cycle?

(def cyclecount 1000000000)
(defn wash-cycles [xs bound]
  (let [bound (long bound)]
    (loop [acc xs
           n   0]
      (if (= n bound)
        acc
        (recur (wash-cycle acc) (unchecked-inc n))))))

(defn find-wash-cycle [xs bound]
  (let [^java.util.Map
        known (java.util.HashMap.)
        ^java.util.Map
        stored (java.util.HashMap.)
        bound (long bound)]
    (loop [acc xs
           n   1]
      (if (= n bound)
        acc
        (let [nxt  (wash-cycle acc)
              k    (mapv count-rocks acc)]
          (if-let [found (and (.get known k)
                              (= (some-> (.get stored k) second) nxt))]
            (do (println [:cycle-found (first (.get stored k)) n  #_nxt])
                [(first (.get stored k)) n nxt])
            (do (.put known k n)
                (.put stored k [n nxt])
                (recur nxt (unchecked-inc n)))))))))

(defn solve2 [txt]
  (let [ins (parse-input2 txt)
        [l r init] (find-wash-cycle ins cyclecount)
        clength (- r l)
        steps (mod (- cyclecount l) clength)]
    (-> init
        (wash-cycles steps)
         north-load)))
