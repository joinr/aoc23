(ns aoc23.day6
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample
"Time:      7  15   30
Distance:  9  40  200")

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map (fn [ln]
              (let [[l r] (s/split ln #":")]
                [(keyword l) (u/read-as-vector r)])))
       (into {})
       ((fn [{:keys [Time Distance]}]
          (mapv (fn [t d]
                  {:time t :distance d}) Time Distance)))))

;;we trade hold for time and speed.
;;t = racetime - hold
;;speed = hold
;;distance = speed * t

;;we have to beat the listed time.  So that prunes the search immediately.
;;Betting we can solve for init speed and then search up/down hold times.

;;also bounded by race time.

;;we can memoize prior values in between even if we bf* it.


;;just bf it to start then pay the toll.

(defn distance [racetime hold]
  (let [speed hold
        traveltime (- racetime hold)]
    (* traveltime speed)))

(defn holds [racetime]
  (for [i (range 1 racetime)]
    {:hold i :distance (distance racetime i)}))

(defn ways-to-win [races]
  (->> (for [{:keys [time distance]} races]
         (->> (holds time)
              (filter #(-> % :distance (> distance)))
              count))
       (reduce *)))

(defn solve1 []
  (->> (u/slurp-resource "day6.txt")
       parse-input
       ways-to-win))
;;Discarding holds that lead to zeros (0 and racetime),
;;we note that these values are symmetric about the
;;midpoint.  This leads to interesting strats depending
;;on pt 2.  We can trivially BF these for now, but
;;a more efficient approach is to examine 1/2 of the
;;search space. Find the cutoff where our distance exceeds (
;;e.g. using binary search).
;;infer the set of values between cutoff and mid point.
;;multiply that by 2.

;;odd times, the midpoint is shared between two values.
;;still ends up symmetric (even easier).


(def sample2
  "Time:      71530
Distance:  940200")

;;we know it's monotonic on either side
;;of the midpoint.

(defn parse-input2 [txt]
  (let [[t d] (->> txt
                   s/split-lines
                   (map (fn [ln]
                          (let [[l r] (s/split ln #":")]
                            (s/replace r #" " "")))))]
    {:time (parse-long  t)
     :distance (parse-long d)}))

(defn midpoint [n]
  (quot n 2))

;;just need to search left of midpoint to find
;;where we are < than distance.  This isn't
;;looking for a specific value, but either
;;that value, or the value nearest to it.
;;trying to find minimum l where (f l) is
;;> 291117211762026 (tgt).

(defn bisearch [f tgt l r]
  (let [cnt (atom 0)]
    (loop [l  l
           vl (f l)
           r   r
           vr (f r)]
      (if (= (swap! cnt inc) 30)
        (println [:exhausted l vl r vr]) ;;sanity check.
        (if (= l (dec r)) ;;adjacent
          {:found nil :l [l vl] :r [r vr]}
          (let [nxt   (+ l (quot (- r l) 2))
                vnxt  (f nxt)
                dir (cond (< vnxt tgt) :<
                          (> vnxt tgt) :>
                          :else :=)
                _ (println [:explore l vl [nxt vnxt dir] r vr])]
            (cond (> vnxt tgt) (recur l vl nxt vnxt)
                  (= vnxt tgt) {:found  [nxt vnxt] :l [l vl] :r [r vr]}
                  (< vnxt tgt) (recur nxt vnxt r vr))))))))


(defn find-min [race-time target-distance]
  (bisearch (fn [n] (distance race-time n)) target-distance 1 (midpoint (dec race-time))))

;;we know our input is odd.  There is another case if it's even.
(defn range-ways-to-win [{:keys [time distance]}]
  (loop [{:keys [found l r]} (find-min time (inc distance))]
    (if-not found ;we have an adjacent pair.  r is the hold time of least value that exceeds the distance.
      (let [[hold-time _] r
            total (inc (-  time (* 2 hold-time)))] ;;since we're symmetric, just subtract this from max hold time.
        total)
      (recur (bisearch (fn [n] (aoc23.day6/distance time n)) distance  (l 0) (found  0))))))

(defn solve2 []
  (->> (u/slurp-resource "day6.txt")
       parse-input2
       (range-ways-to-win)))
