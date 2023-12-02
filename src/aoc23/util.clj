(ns aoc23.util
  (:require [clojure.java.io :as jio]))

(defn slurp-resource [path]
  (slurp (jio/resource "day1.txt")))

