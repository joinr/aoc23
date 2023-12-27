(ns aoc23.day19
  (:require [aoc23.util :as u]
            [clojure.string :as s]
            [clojure.walk :as w]))

(def sample
"px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")


(defn chomp [^String s delim]
  (let [n (s/index-of s delim)]
    [(subs s 0 n) (subs s (inc n))]))

(def ops {\< <
          \> >})

(def terminals {"A" :A
                "R" :R})

(defn parse-if [txt]
  (let [[l op]  (subs txt 0 2)
        n (parse-long (subs txt 2))]
    [(keyword (str l)) op n]))

;;inside the braces.
(defn parse-rule [ln]
  (let [[ifo body]    (chomp ln ":")
        [theno elseo] (chomp body ",")
        theno (keyword theno)
        ifo (parse-if ifo)]
    (if (s/includes? elseo ":")
      [ifo theno (parse-rule elseo)]
      [ifo theno (keyword elseo)])))

#_
(defn deps [[op l r]]
  (->> (if (vector? r)
         (into [l] (deps r))
         [l r])
       (keep #(case %
                (:A :R) nil
                %))
       vec))

#_
(defn rule-deps [db]
  (reduce-kv (fn [acc parent rule]
              (assoc acc parent (deps rule)))
             {} db))


(defn parse-entry [ln]
  (let [[k v] (chomp ln "{")]
    [(keyword k) (parse-rule (subs v 0 (dec (count v))))]))

(defn parse-piece [ln]
  (-> ln
      (s/replace #"[xmas=]" {"x" ":x" "m" ":m" "a" ":a" "s" ":s" "=" " "})
       clojure.edn/read-string ))

(defn parse-input [txt]
  (let [[rules pieces]  (-> txt (s/split #"\n\n|\r\n\r\n"))]
    {:rules  (->> rules s/split-lines (map parse-entry) (into {}))
     :pieces (mapv parse-piece (s/split-lines pieces))}))

(def root #{:A :R})

(defrecord mop [k op f n]
  clojure.lang.IFn
  (invoke [this m] (f (get m k) n)))

(defn ->match [k op n]
  (->mop k op (ops op) n))

(defn rule-tree [db parent]
  (let [[ifo l r] (if (vector? parent) parent (db parent))]
    {:match (apply ->match ifo)
     :l (or (root l) (rule-tree db l))
     :r    (or (root r) (rule-tree db r))}))

(defn match [part rt]
  (let [{:keys [match l r]} rt]
    (if (match part)
      (or (root l) (recur part l))
      (or (root r) (recur part r)))))

(defn solve1 [txt]
  (let [{:keys [rules pieces]}  (->> txt parse-input)
        rt (rule-tree rules :in)]
    (->> pieces
         (map (fn [p] (assoc p :match (match p rt))))
         (group-by :match)
         :A
         (map (fn [m] (->> (dissoc m :match) vals (reduce +))))
         (reduce +))))

;;part 2
;;solution is to use the tree to compute intervals for xmas.
;;starting from {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}
;;compute a walk down the tree until we hit an A terminal.
;;At each step we have modified the criteria.  Should end up with
;;map of ranges.


(defn split-interval [[l r] op n]
  (case op
    \<   [[l (dec n)] [n r]]
    \>   [[(inc n) r] [l n]]))

(def inits {:x [1 4000]
            :m [1 4000]
            :a [1 4000]
            :s [1 4000]})

(defn walk-intervals [tree remaining]
  (case tree
    :A [remaining]
    :R []
    (let [{:keys [match l r]} tree
          {:keys [k op n]} match
          [cl cr] (split-interval (remaining k) op n)
          reml    (assoc remaining k cl)
          remr    (assoc remaining k cr)]
      (into (walk-intervals l reml) (walk-intervals r remr)))))

(defn combos [xs]
  (->> xs
       (map (fn [m]
              (->> m
                   vals
                   (map (fn [[l r]] (inc (- r l))))
                   (reduce *))))
       (reduce +)))

(defn solve2 [txt]
  (let [{:keys [rules]}  (->> txt parse-input)
        rt (rule-tree rules :in)]
    (->>  (walk-intervals rt inits)
          combos)))
