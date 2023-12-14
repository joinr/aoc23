(ns aoc23.util
  (:require [clojure.java.io :as jio])
  (:import [java.util ArrayDeque]))

(defn slurp-resource [path]
  (slurp (jio/resource path)))

(defn enclose [s char]
  (case char
    \[ (str \[ s \])
    \{ (str \{ s \})
    \( (str \( s \))
    (str char s char)))

(defn brackets [s]
  (enclose s \[))

(defn braces [s]
  (enclose s \{))

(defn read-as-vector [txt]
  (-> txt
      brackets
      clojure.edn/read-string))

(defn map-kv [fkv m]
  (reduce-kv (fn [acc k v]
            (assoc acc k (fkv k v))) m m))

(defn memo-1 [f]
  (let [^java.util.HashMap cache (java.util.HashMap.)]
    (fn [x]
      (if-let [res (.get cache x)]
        res
        (let [res (f x)
              _ (.put cache x res)]
          res)))))

(defn float= [l r]
  (let [diff (- l r)]
    (if (< (Math/abs diff) 10E-6)
      true
      false)))

(defmacro finite-loop [n bindings & body]
  `(let [limit# (atom 0)
         n# ~n]
     (loop ~bindings
       (if (< (swap! limit# inc) n#)
         ~@body
         (println [:exceded-limit])))))

(def ^:dynamic *logging* nil)
#_
(defn log [x]
  (when *logging*
    (println x))
  x)

(defmacro log [msg & expr]
  `(do (when *logging*
         (println ~msg))
       ~@expr))


(definterface I2D
  (getEntry ^long [^long r ^long c])
  (setEntry [^long r ^long c ^long v]))

;;redundant but meh.
(defn get-entry ^long [^I2D gr ^long x ^long y]
  (.getEntry gr x y))

(defn set-entry [^I2D gr ^long x ^long y ^long v]
  (.setEntry gr x y v))

;;no bounds checking lol.
(defrecord grid [^long w ^long h ^longs entries]
  I2D
  (getEntry ^long [this ^long x ^long y]
    (aget entries (+ (* y w) x)))
  (setEntry ^long [this ^long x ^long y ^long v]
    (do (aset entries (+ (* y w) x) v)
        this)))

(defn new-grid
  (^grid [^long w ^long h xs]
   (let [entries (if xs (long-array xs)
                     (long-array (* w h)))]
     (->grid w h entries)))
  (^grid [w h] (new-grid w h nil)))

(defn valid-entry ^long [^grid g ^long x ^long y ^long not-found]
  (if (and (> y -1) (< y (.-h g))
           (> x -1) (< x (.-w g)))
    (get-entry g x y)
    not-found))


(defn neighbors [x y]
  [[(inc x) y] ;;r
   [(dec x) y] ;;l
   [x (inc y)] ;;u
   [x (dec y)] ;;d
   [(inc x) (inc y)] ;;ur
   [(dec x) (inc y)] ;;ul
   [(dec x) (dec y)] ;;dl
   [(inc x) (dec y)] ;;dr
   ])

(defn neighbors4 [x y]
  [[(inc x) y] ;;r
   [(dec x) y] ;;l
   [x (inc y)] ;;u
   [x (dec y)] ;;d
   ])

(defn idx->xy [w h n]
  [(rem n w) (quot n h)])

(defn xy->idx [w h x y]
  (+ x (* y w)))

(defn adjacency [w h & {:keys [neighbors-fn] :or
                        {neighbors-fn neighbors}}]
  (->> (for [x (range w)
             y (range h)]
         (let [nebs (->> (neighbors-fn x y)
                         (filterv (fn [[x y]]
                                    (and (>= x 0) (< x w)
                                         (>= y 0) (< y h))))
                         (mapv (fn [[x y]]
                                (xy->idx w h x y))))]
           [(xy->idx w h x y) [x y] nebs]))
       (reduce (fn [acc [n xy nebs]]
                 (assoc acc n {:coord xy :neighbors nebs})) {})))

;;minimally inspired by spork.cljgraph
(defprotocol IFringe
  (peek-fringe [fr])
  (pop-fringe  [fr])
  (push-fringe [fr w nd]))


(defrecord mutq [^ArrayDeque q]
  IFringe
  (peek-fringe [fr] (.peek q))
  (pop-fringe [fr] (.poll q) fr)
  (push-fringe [fr w nd] (.add q nd) fr))

(defn q [& xs]
  (reduce (fn [acc e]
            (push-fringe acc (e 0) (e 1)))
          (mutq. (ArrayDeque.)) xs))

;;original lame priorityq impl.
(defrecord minpq [entries]
  IFringe
  (peek-fringe [fr]
    (when-let [v (first entries)]
      (v 1)))
  (pop-fringe  [fr]
    (if-let [k (first entries)]
      (minpq. (disj entries k))
      fr))
  (push-fringe [fr w nd]
    (minpq. (conj entries [w nd]))))

(defn min-pq [& xs]
  (minpq. (into (sorted-set-by
                 (fn [l r]
                   (let [res (compare (l 0) (r 0))]
                     (if-not (zero? res) res
                             (compare (l 1) (r 1)))))) xs)))

;;playing with indexed priority queue to see if we do better.
(defrecord minidxq [entries known]
  IFringe
  (peek-fringe [fr]
    (when-let [v (first (vals entries))]
      (first v)))
  (pop-fringe  [fr]
    (if-let [kv (first entries)]
      (let [k  (key kv)
            xs (val kv)
            v (first xs)]
        (minidxq. (if (> (count xs) 1)
                 (assoc  entries k (disj xs v))
                 (dissoc entries k))
               (dissoc known v)))
        fr))
  (push-fringe [fr k v]
    (if-let [oldk (known v)]
      (let [old     (entries oldk)]
        (minidxq. (-> (if (> (count old) 1)
                     (assoc entries oldk (disj old v))
                     (dissoc entries oldk))
                   (update k #(conj (or % #{}) v)))
               (assoc known v k)))
      (minidxq. (update entries k #(conj (or % #{}) v))
             (assoc known v k)))))

(defn min-indexed-pq [& xs]
  (let [groups (group-by first xs)
        known (reduce (fn [acc [w k]]
                        (assoc acc k w)) {} xs)]
    (minidxq. (into (sorted-map) (for [[k v] groups]
                                [k (set (map second v))]))
           known)))





;;graph stuff.
;;===========
(defn multipath [x y]
  (if (set? x)
    (conj x y)
    (conj #{x} y)))

(defn relax
  ([{:keys [spt dist fringe] :as state} source sink w hw]
   (let [dfrom (or (dist source) (throw (ex-info "unknown weight!" {:in dist :source source})))
         wnew  (+ ^long dfrom ^long w)]
     (if-let [^long wold (dist sink)] ;;known path.
       (cond (< wnew wold) ;;shorter path
             {:spt    (assoc spt sink source)
              :dist   (assoc dist sink wnew)
              :fringe (push-fringe fringe (+  wnew ^long hw) sink)}
             (= wnew wold) ;;equal path
             {:spt  (update spt sink multipath source)
              :dist dist
              :fringe fringe}
             :else ;;no change
             state)
       ;;new path
       {:spt    (assoc spt  sink source)
        :dist   (assoc dist sink wnew)
        :fringe (push-fringe fringe (+ wnew ^long hw) sink)})))
  ([state source sink w] (relax state source sink w 0)))


(defn best-first [gr from to & {:keys [make-fringe on-visit weightf]
                                :or {make-fringe min-pq
                                     on-visit identity}}]
  (let [sinks (gr :sinks)
        nodes (gr :nodes)
        weightf (or weightf
                    (fn [nd] ((nodes nd) :weight)))
        init {:spt  {from from}
              :dist {from (weightf from)}
              :fringe (make-fringe [0 from])}]
    (loop [{:keys [spt dist fringe] :as state} init]
      (if-let [source (peek-fringe fringe)]
        (if (= source to)
          (assoc state :found-path [from to])
          (let [_ (on-visit source)
                neighbors  (sinks source)
                next-state (reduce-kv (fn [acc sink w]
                                        (relax acc source sink w))
                                      (update state :fringe pop-fringe)
                                      (sinks source))]
            (recur next-state)))
        state))))


(defn a* [gr from to h & {:keys [make-fringe on-visit weightf]
                          :or {make-fringe min-pq
                               on-visit identity}}]
  (let [sinks (gr :sinks)
        nodes (gr :nodes)
        weightf (or weightf
                    (fn [nd] ((nodes nd) :weight)))
        init {:spt  {from from}
              :dist {from (weightf from)}
              :fringe (make-fringe [0 from])}]
    (loop [{:keys [spt dist fringe] :as state} init]
      (if-let [source (peek-fringe fringe)]
        (if (= source to)
          (assoc state :found-path [from to])
          (let [_ (on-visit source)
                neighbors  (sinks source)
                next-state (reduce-kv (fn [acc sink w]
                                        (let [hw (h sink to)]
                                          (relax acc source sink w hw)))
                                      (update state :fringe pop-fringe)
                                      (sinks source))]
            (recur next-state)))
        state))))

(defn recover-first-path
  ([{:keys [spt dist]} from to]
   (let [xs (->> to
                 (iterate (fn [nd] (let [pred (spt nd)]
                                     (if (coll? pred)
                                       (first pred)
                                       pred))))
                 (take-while #(not= % from))
                 reverse
                 (into [from]))]
     ;;need to remove the starting position cost
     {:path xs :length (- ^long (dist to) ^long (dist from))}))
  ([{:keys [found-path] :as state}]
   (when-let [[from to] found-path]
     (recover-first-path state from to))))

(defn branch? [x] (set? x))

(defn backtrack [preds startnode tail-path]
 (when (seq tail-path)
    (loop [path          tail-path
           pending-paths []]
      (let [node (first path)]
        (if (= node startnode) [path pending-paths]
            (let [prior  (get preds node)
                  prior-node   (if (branch? prior) (first prior) prior)
                  branch-paths (when (branch? prior)
                                 (reduce (fn [acc x] (conj acc (cons x path))) [] (rest prior)))]
              (recur (cons prior-node path)
                     (reduce (fn [acc x] (conj acc x))  pending-paths branch-paths))))))))

(defn paths 
  "Given a shortest-path-tree that encodes the predecessors of nodes, yield a 
   sequence of all the paths from start node to end node."
  [preds startnode endnode]
  (map first 
       (->> (iterate (fn [pair]
                       (let [current-path  (nth pair 0)
                             pending-paths (nth pair 1)]
                         (when (not (empty? pending-paths))
                           (let [res (backtrack preds startnode 
                                                (first pending-paths))
                                 next-path    (nth res 0)
                                 new-subpaths (nth res 1)]
                             (vector next-path (into (rest pending-paths) new-subpaths))))))
                     (backtrack preds startnode (list endnode)))
            (take-while identity))))

(defn manhattan [w h from to]
  (let [xy1 (idx->xy w h from)
        xy2 (idx->xy w h to)]
    (+ (Math/abs (- ^long (xy2 0) ^long (xy1 0)))
       (Math/abs (- ^long (xy2 1) ^long (xy1 1))))))


(defn manhattan-xy [xy1 xy2]
    (+ (Math/abs (- ^long (xy2 0) ^long (xy1 0)))
       (Math/abs (- ^long (xy2 1) ^long (xy1 1)))))

;;given a 1d sequence of entries, corresponding to a grid of w h,
;;unrolled into a 1d indexing scheme, create a corresponding
;;graph where the adjacency is determined by the neighborhood
;;function (typically 4 way or 8 way neighbors), with the weights
;;supplied by entries.
(defn entries->graph [entries w h & {:keys [neighbors-fn] :or {neighbors-fn neighbors4}}]
  (let [adj (adjacency w h :neighbors-fn neighbors-fn)]
    (reduce-kv (fn [acc nd {:keys [coord neighbors]}]
                 (-> acc
                     (assoc-in [:nodes nd] {:weight  (entries nd)
                                            :coord coord})
                     (assoc-in [:sinks nd]
                               (reduce (fn [acc k]
                                         (assoc acc k (entries k))) {} neighbors))))
               {:width w :height h} adj)))


;;just pulled a single function:

;;https://github.com/clojure/math.combinatorics/blob/master/src/main/clojure/clojure/math/combinatorics.cljc
;;   Copyright (c) Mark Engleberg, Rich Hickey and contributors. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;; combinatorics.clj: efficient, functional algorithms for generating lazy
;;; sequences for common combinatorial functions.

;; by Mark Engelberg (mark.engelberg@gmail.com)
;; Last updated - July 24, 2019
(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                        (if-let [rst (next (v-seqs i))]
                          (assoc v-seqs i rst)
                          (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))
s
