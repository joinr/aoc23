(ns aoc23.day3
  (:require [clojure.string :as s]
            [aoc23.util :as u]))

(def sample
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")


;;want to know where things start/stop
;;want to get a vector of [{:number 467 :span [0 2]} {:number 114 :span [5 7]}]

(def nums (into #{} (seq "1234567890")))

;;ugly but works.
;;TODO use regex + interop for matching with indices.
(defn extract-syms [xs]
  (let [res (reduce (fn [{:keys [items current iprev] :as acc} [i c]]
              (let [emit-symbol?  (not (nums c))
                    emit-number?  (and current
                                      (or emit-symbol?
                                          (not= iprev (dec i))))
                    accnxt (assoc acc :iprev i)]
                (cond
                  (and emit-symbol? emit-number?)
                    (-> accnxt
                        (update :items conj
                                (assoc current :end iprev)
                                {:start i :string (str c) :end i})
                        (assoc :current nil))
                  emit-symbol?
                  (let [itemsnxt (-> (if current
                                       (conj items
                                             (assoc current :end iprev))
                                       items)
                                     (conj {:start i :string (str c) :end i}))]
                    (-> accnxt
                        (assoc :items itemsnxt)
                        (assoc :current nil)))
                  emit-number?
                    ;;new number.
                    (-> accnxt
                        (update :items conj (assoc current :end iprev))
                        (assoc :current {:start i :string (str c)}))
                  :else ;accumulate number
                    (assoc accnxt :current (if current
                                             (update current :string str c)
                                             {:start i :string (str c)})))))
                {:items [] :current nil :iprev -1} xs)]
    (if (res :current)
      (let [final (res :current)]
        (conj (res :items) (assoc final :end
                                  (+ (final :start) (count (final :string))))))
      (res :items))))


(defn parse-line [ln]
  (->> ln
       (map-indexed (fn [i x] [i x]))
       (filter #(not= (second %) \.))))

(defn symbolic [x]
  (and (string? x)
       (= (count x) 1)
       (not (nums (nth x 0)))))

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map-indexed (fn [idx ln]
                      (->> ln  parse-line extract-syms (map (fn [m] (assoc m :row idx))))))
       (apply concat)
       (map (fn [{:keys [string] :as m}]
              (-> m
                  (assoc :data (if (symbolic string) string (parse-long string)))
                  (dissoc :string))))))

;;possible bug depending on input.  numbers are not guaranteed to be distinct in the
;;dataset.  this assumes they are.  lucked out.
;;robust solution projects the entries onto some distinct key,
;;then collects by distinct id for any operations.

;;we now store a node with a distinct id for neighbor queries.
(defn as-2d [rows] ;;row major orientation.
  (let [id (atom -1)]
    (reduce (fn [acc {:keys [start end row data]}]
              (let [oldrow (get acc row {})
                    node  {:id (swap! id inc)
                           :data data}
                    inner (reduce (fn [acc j]
                                    (assoc acc j node))
                                  oldrow (range start (inc end)))]
                (assoc acc row inner))) {} rows)))

;;distinct nodes by id key now.
(defn distinct-by-id [xs]
  (->> xs
       (reduce (fn [acc x]
                 (if (acc (x :id))
                   acc
                   (assoc acc (x :id) x)))
               {})
       vals))
;;lift data id.
(defn adjacents [x y grid]
  (->> (u/neighbors x y)
       (map (fn [[x y]] (get-in grid [y x])))
       (filter #(-> % :data number?))
       distinct-by-id
       (map :data)))

(defn parts-ctx [rows]
  (let [symbols (->> rows (filter (fn [{:keys [data]}] (string? data)))
                     (mapv (fn [{:keys [start row data]}]
                             [[start row]  data])))] ;;col major
    {:symbols (into {} symbols) ;;col-major
     :grid (as-2d rows) ;;row-major
     }))

(defn find-parts [{:keys [symbols grid] :as ctx}]
  (for [[[x y] symb] symbols]
    (adjacents x y grid)))

(defn solve1 [txt]
  (->> (parse-input txt)
       parts-ctx
       find-parts
       (filter seq)
       (apply concat)
       (reduce +)))

;;part 2
(defn find-gears [{:keys [symbols grid] :as ctx}]
  (->> (for [[[x y] symb] symbols
             :when (= symb "*")]
         (let [res (adjacents x y grid)]
           (when (>= (count res) 2)
             res)))
       (filter identity)))

(defn solve2 [txt]
  (->> (parse-input txt)
       parts-ctx
       find-gears
       (filter seq)
       (map (fn [xs] (reduce * xs)))
       (reduce +)))

;;define adjacency

;;symbols are just [x,y].
;;so we can look for adjacency by looking to see if any value around them is a digit.

;;given 3,1 are there any symbols in the preceding line that intersect its neighborhood?

;;intersection query can be limited to the row above and below the symbol.

;; 467..114..
;; ...*......
;; ..35..633.


;;if point is in the same row, only check l/r
;;above -> u,ul,ur
;;below -> d,dl,dr


;;we can creat an implicit grid.
;;just walk the rows and expand them into a sparse map.

;;then we get trivial adj queries.
;;we only need 3 rows at a time, rolling window, to answer adjacency queries.





