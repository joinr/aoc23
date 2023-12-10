(ns aoc23.day10
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample
".....
.S-7.
.|.|.
.L-J.
.....")

;;destinations for four cardinal directions.
(def lefts  #{\-  \L  \F \S})
(def rights #{\-  \J  \7 \S} )
(def ups    #{\|  \F  \7 \S} )
(def downs  #{\|  \J  \L \S})

;;leaving from this char, which chars
;;are valid in each of the directions?
(def valid
  {\|  {:u ups
        :d downs}
   \-  {:l lefts
        :r rights}
   \L  {:u ups
        :r rights}
   \F  {:d downs
        :r rights}
   \J  {:u ups
        :l lefts}
   \7  {:d downs
        :l lefts}
   \S  {:u ups
        :d downs
        :l lefts
        :r rights}
   \.  {}})

(defn pipe-neighbor [data x y]
  (let [[right left up down :as nebs]   (u/neighbors4 x y)
        [rc lc uc dc :as cs] (mapv (fn [[x y]]
                                     (get-in data [y x])) nebs)
        c             (get-in data [y x]) ;;row major ugh!
        {:keys [u d l r]} (valid c)]
    (if (not (or u d l r))
      #{}
      (let [viables [(when (and r rc (r rc)) right)
                     (when (and l lc (l lc)) left)
                     (when (and u uc (u uc)) up)
                     (when (and d dc (d dc)) down)]]
        (println [{:c c :x x :y y :nebs nebs :cs cs} lc [c] rc   uc [c] dc r l u d viables])
        ;;now merge the connectedness together.
        (->> viables
             (filter identity)
             set)))))

(defn parse-input [txt]
  (let [data (->> txt
                   s/split-lines
                   (mapv vec))
        grid (->> (u/adjacency (count (first data)) (count data)
                               :neighbors-fn #(pipe-neighbor data %1 %2))
                  (reduce-kv (fn [acc k {:keys [coord] :as node}]
                               (assoc acc k (assoc node :data (get-in data coord))))
                             {}))]
    {:init (->> grid (some (fn [[k {:keys [data]}]]
                             (when (= data \S)
                               k)))) :grid grid}))

(defn depth-walk [{:keys [grid init]}]
  (u/finite-loop 25
      [pending (list {:path [init] :visited #{} :distance 0})]
    (if-let [nxt (first pending)]
      (let [{:keys [path visited distance]} nxt
            current (peek path)]
        (if (and (= current init) (pos? distance))
          nxt
          (let [_       (println nxt)
                visited (conj visited current)
                blocks  (-> grid (get-in [current :data]) blocked)
                nebs    (->> (get-in grid [current :neighbors])
                             (filter (fn [n] (not (visited n)))))]
            (recur (into pending (map (fn [neb]
                                        {:path (conj path neb)
                                         :visited (conj visited neb)
                                         :distance (inc distance)})
                                      nebs))))))
      (println [:failed]))))
