(ns aoc23.day8
  (:require [aoc23.util :as u]
            [clojure.string :as s]))

(def sample
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(defn parse-input [txt]
  (let [[ins _ & xs ] (s/split-lines txt)]
    {:directions ins
     :nodes
     (->> xs
          (map #(-> % (s/split #" = ")))
          (mapv (fn [[parent child]]
                  [parent (-> child (s/replace  #"\(|\)" "") (s/split #", "))]))
          (into {}))}))

(defn get-child [children c]
  (children (if (= c \R) 1 0)))

(defn step
  ([nodes idx bnd ^String dir stop? parent]
   (if (stop? parent)
     {:found idx :parent parent}
     (if  (< idx bnd)
       (let [d        (.charAt dir idx)
             nxt      (-> parent nodes  (get-child d))]
         (recur nodes (unchecked-inc idx) bnd dir stop? nxt))
       {:not-found idx :parent parent})))
  ([{:keys [directions nodes]}  stop? parent]
   (step nodes 0 (count directions) directions stop? parent))
  ([{:keys [directions nodes]} parent]
   (step nodes 0 (count directions) directions  #{"ZZZ"} parent))
  ([{:keys [directions nodes]}]
   (step nodes 0 (count directions) directions  #{"ZZZ"}  "AAA")))

(defn travel [ctx target start]
  (let [stop? (if (or (set? target) (fn? target))
               target
               #{target})]
    (loop [n      0
           parent start]
      (let [res (step ctx stop? parent)]
        (if-let [idx (res :found)]
          (+ n (long idx))
          (do #_(println [:not-found n :from parent :ended (res :parent)])
              (recur (+ n (long (res :not-found))) (res :parent))))))))

;;if we visit a parent we've already seen, we will never hit the target.
;;there are probabaly cycles.
(def sample2
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defn solve1 []
  (-> (u/slurp-resource "day8.txt")
      parse-input
      (travel "ZZZ" "AAA")))


;;part 2


(def sample3
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")


(defn walk-cycle [nodes init ^String dir]
  (let [bnd (count dir)]
    (loop [visited     {}
           terminals   {}
           parent      init
           idx         0
           n           0]
        (if (visited [parent idx])
          {:cycle-start {:pidx [parent idx]
                         :origin (visited [parent idx])
                         :n n
                         :length (- n (visited [parent idx]))}
           :visited visited
           :n n
           :idx idx
           :terms terminals}
          (let [d (.charAt dir idx)
                ^String
                chld (-> parent nodes (get-child d))
                nnxt  (unchecked-inc n)
                terms (if (= (.charAt chld 2) \Z)
                        (assoc terminals nnxt chld)
                        terminals)
                vnxt (assoc visited [parent idx] nnxt)]
            (recur vnxt terms chld (if (= idx (unchecked-dec bnd))
                                     0
                                     (unchecked-inc idx)) nnxt))))))

(defn seeds [ctx]
  (->> ctx :nodes keys (filter (fn [x] (= (last x) \A)))))

(defn cycles [{:keys [nodes directions] :as ctx}]
  (->> ctx
       seeds
       (map (fn [nd]
              (let [res (walk-cycle nodes nd directions)
                    card (->> res :visited keys (map first) distinct count)]
              [nd (assoc res :card card)])))
       (into {})))

(defn normalized-terminals [xs]
  (sort (for [[_ {:keys [terms]}] xs]
                     (-> terms keys first))))

;;https://github.com/morrxy/4clojure/blob/master/problem/100.Least%20Common%20Multiple.clj
(defn lcm  [& x]
  (let
      [gcd (fn gcd [a b] (if (= 0 b) a (gcd b (mod a b))))
       lcm (fn lcm [a b] (/ (* a b) (gcd a b)))]
    (reduce lcm x)))

;;compute how far along a path
(defn solve2 []
  (->> (u/slurp-resource "day8.txt")
       parse-input
       cycles
       normalized-terminals
       (apply lcm)))
