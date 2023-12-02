(ns aoc23.day1
  (:require [clojure.java.io :as jio]))

(def sample
  "1abc2
   pqr3stu8vwx
   a1b2c3d4e5f
   treb7uchet")

(def nums (set (seq "1234567890")))
(defn line->digits [^String ln]
  (let [l (->> ln (some nums))
        r (->> ln reverse (some nums))]
    (parse-long (str l r))))

(defn solve [txt]
  (->> txt
       clojure.string/split-lines
       (map line->digits)
       (reduce +)))

(defn solve1 []
  (->> (jio/resource "day1.txt")
       slurp
       solve))

;;part 2
;;expand digit parsing to inclue english names.
;;only parse the first digit still.  combine this with our
;;nums parse.


(def engnums
  {"six" "6",
   "three" "3",
   "two" "2",
   "seven" "7",
   "five" "5",
   "eight" "8",
   "one" "1",
   "nine" "9",
   "four" "4"})

(def revnums (zipmap (map clojure.string/reverse (keys engnums)) (vals engnums)))

(def num-pat #"one|two|three|four|five|six|seven|eight|nine")
(def rev-pat (re-pattern (clojure.string/reverse  (str num-pat))))

;;we have 2 case:

;;either a 1 char match, which is a number,
;;or we need to look ahead n chars to see if we match another.
;;we might be walking backwards too.

;;for now just brute force it.  parse the whole line in single pass.
;;replace words with digits. then use our original algo.


(def sample2
  "two1nine
  eightwothree
  abcone2threexyz
  xtwone3four
  4nineeightseven2
  zoneight234
  7pqrstsixteen")

;;want to block scan the sequence, try to parse
(defn anglicize [pat m txt] (clojure.string/replace txt pat m))
(defn ang-digits [ln]
  (let [l  (->> ln (anglicize  num-pat engnums) (some nums))
        r  (->>  ln clojure.string/reverse (anglicize rev-pat revnums) (some nums))]
    (parse-long (str l r))))

(defn solve2 [txt]
  (->> txt
       clojure.string/split-lines
       (map ang-digits)
       (reduce +)))

