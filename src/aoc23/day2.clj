(ns aoc23.day2
  (:require [clojure.java.io :as jio]))

(def init
  {:r 12
   :g 13
   :b 14})


(def colors
  {"red"   :r
   "green" :g
   "blue"  :b})

(def sample
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn parse-draws [xs]
  (->> (for [[l r] (->> (clojure.string/split xs #",")
                        (map clojure.string/trim)
                        (map #(clojure.string/split % #" ")))]
         [(colors r) (parse-long l)])
       (into {})))

(defn game-bounds [{:keys [draws]}]
  (reduce (fn [acc m]
            (reduce-kv (fn [bnds color x]
                         (if-let [[l u :as cb] (bnds color)]
                           (assoc bnds color [(min l x) (max u x)])
                           (assoc bnds color [x x])))
                       acc m)) {} draws))
(defn parse-game [ln]
  (let [[g xs] (clojure.string/split ln #":")
        draws (clojure.string/split xs #";")
        g     {:game (parse-long (clojure.string/replace g "Game " ""))
               :draws (mapv parse-draws draws)}]
    (assoc g :bounds (game-bounds g))))

;;do any games have a bounds where either the maximum seen
;;is greater than the color's init?

(defn feasible? [init {:keys [bounds]}]
  (reduce-kv (fn [acc color n]
               (let [[l u] (bounds color)]
                 (if (not (<=  u n))
                   (reduced false)
                   true)))
             true init))
;;compute the bounds.  for any single draw in a game we have a maximum.


(defn parse-input [txt]
  (->> txt
       clojure.string/split-lines
       (mapv parse-game)))

(defn solve1 []
  (->> (slurp (jio/resource "day2.txt"))
       parse-input
       (filter #(feasible? init %))
       (map :game)
       (reduce +)))


;;part 2

(defn power  [{:keys [bounds]}]
  (->> bounds vals (map second) (reduce *)))

(defn solve2 []
  (->> (slurp (jio/resource "day2.txt"))
       parse-input
       (map power)
       (reduce +)))
