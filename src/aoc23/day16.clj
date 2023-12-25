(ns aoc23.day16
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample (u/slurp-resource "day16sample.txt"))

;; .|...\....
;; |.-.\.....
;; .....|-...
;; ........|.
;; ..........
;; .........\
;; ..../.\\..
;; .-.-/..|..
;; .|....-|.\
;; ..//.|....



;;naive.
;;neighbors depend on direction of travel and coordinate.
;;also need to account for bounds.
;;can pre-process the grid into a higher order navgraph.
;;navgraph contains information about hops (could also memoize)
;;the implicit neighbors when we discover them).

;;Goal is to eventually retrace the steps. and determine coverage.

(defn parse-input [txt]
  (let [xs   (->> txt
                  s/split-lines
                  (mapv vec))
        w (count (first xs))
        h (count xs)]
    #_(u/entries->graph (reduce into [] xs) w h )
    {:w w :h h :entries xs :coord->data (fn [xy] ((xs (xy 1)) (xy 0)))}))

(defn shift [dir [x y]]
  (case dir
    :n [x (dec y) #_(inc y)]  ;;need to swap these.
    :s [x (inc y) #_(dec y)]
    :e [(inc x) y]
    :w [(dec x) y]))

(defn clamp [w h [x y :as coord]]
  (when (and (<  -1 x  w)
             (<  -1 y  h))
    coord))

;;should have only 1 neighbor since we are
;;directional.
(defn neighbors [w h dir coord coord->data]
  (when-let [new-coord (clamp w h (shift dir coord))]
    (let [data (coord->data new-coord)]
      (case data
        \. [dir new-coord]
        \| (case dir
             (:n :s) [dir new-coord]
             {:split [[:n new-coord]
                      [:s new-coord]]})
        \- (case dir
             (:e :w) [dir new-coord]
             {:split [[:e new-coord]
                      [:w new-coord]]})
        \/ [(case dir :e :n, :w :s, :n :e , :s :w)
            new-coord]
        \\ [(case dir :e :s, :w :n, :n :w , :s :e) new-coord]))))


;;need to change this from a path enumeration to a simple back
;;tracking depth walk.  We need to enumerate reachable nodes.
;;Our visitation rules are based on both direction and the coord.

(defn node-walk
  [{:keys [w h entries coord->data] :as ctx} dir start]
   (loop [pending    (list  [dir start]) ;;nil if noone to follow.
          visited    #{}]
       (if-let [node (first pending)]
         (if (vector? node) ;;normal
           (if (visited node)
             (recur (pop pending)
                    visited)
           (let [[dir xy] node
                 nebs    (neighbors w h dir xy coord->data)]
             (recur (if nebs (conj (pop pending) nebs)
                        (pop pending))
                    (conj visited node))))
         (let [[l r]    (node :split)
                 #_#__ (println [:split node])]
             (recur (-> pending
                        pop
                        (conj l)
                        (conj r))
                    visited)))
         ;;no neighbors.  yield current path.
         (disj visited [dir start]))))

(defn energized [ctx dir start]
  (->> (node-walk ctx dir start)
       (map second)
       distinct
       count))

(defn solve1 [txt]
  (-> txt
      parse-input
      (energized :e [-1 0])))
#_
(solve1 (u/slurp-resource "day16.txt"))

(defn max-energy [{:keys [w h] :as ctx}]
  (->> (concat
              (for [y (range 0 (inc h))]
                [[:e [-1 y]]
                 [:w [(inc w) y]]])
              (for [x (range 0 (inc w))]
                [[:s [x -1]]
                 [:n [x  (inc h)]]]))
       (reduce into [])
       (map (fn [[dir xy]]
              [(energized ctx dir xy) dir xy ]))
       (sort-by first)
       last
       first))

(defn solve2 [txt]
  (-> txt
      parse-input
      max-energy))


;;naive solution is to just walk the edges and find a max
;;value...takes us 20ms to energize so brute force it.




(comment 
;;modified depth walk that takes into account direction.
(defn walk
  ([{:keys [w h entries coord->data] :as ctx} dir start seed]
   (let [{:keys [nxt path visited subpaths]} seed]
     (loop #_#_u/finite-loop 100
           [nxt        (or nxt [dir start]) ;;nil if noone to follow.
            path       (or path [])
            visited    (or visited #{})
            subpaths   (or subpaths [])]
       (if-let [node nxt]
         (if (vector? node) ;;normal
           (if (visited node) ;;cycle
             {:cycle true :path path :visited visited :subpaths subpaths}
             (let [[dir xy] node
                   nebs    (neighbors w h dir xy coord->data)]
               (recur nebs
                      (conj path node)
                      (conj visited node)
                      subpaths)))
           ;;split.
           (let [[l r] (node :split)
                 [dir xy] l]
             (recur (neighbors w h dir xy coord->data)
                    (conj path l)
                    (conj visited l)
                    (conj subpaths {:nxt r :path path  :visited visited}))))
         ;;no neighbors.  yield current path.
         {:path path :visited visited :subpaths subpaths}))))
  ([ctx dir start] (walk ctx dir start {})))

;;maybe keep track of subpaths?
(defn enumerate-paths [ctx dir start]
  (let [{:keys [path visited subpaths cycle] :as init} (walk ctx dir start)]
    (#_loop u/finite-loop 20000
        [pending subpaths
         acc [[(u/restv path) cycle]]]
        (println (count pending))
      (if-let [child (first pending)]
        (let [res  (walk ctx dir start child)]
          (recur
           (into (rest pending) (res :subpaths))
           (conj acc {:path (u/restv (res :path)) :cycle (res :cycle)})))
        acc))))

(defn energized [paths]
  (->> paths
       (mapcat :path)
       (map second)
       (into #{})
       count))
)



















