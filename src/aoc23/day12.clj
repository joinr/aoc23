(ns aoc23.day12
  (:require [aoc23.util :as u]
            [clojure.string :as s]))

(def sample
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

;;I think we can just slide the intervals around and generate
;;strings then compare them with the base.


(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map (fn [ln]
              (s/split ln #" ")))))

;;so 1 1 3 ->
;; "#" "#" "###"

;;walk the string, looking to place the next run.
;;to place a run, have to have space for it.
;;if there's already a character there, all chars must match.

(defn pieces [xs]
  (for [n xs]
    (apply str (repeat n \#))))

;;solution may look like
;;[piece1 idx
;; piece2 idx
;; piece3 idx]

;;if we go char by char,
(defn char-at [^CharSequence s ^long n]
  (.charAt s n))

(defn get-piece ^String [^long n]
  (apply str (repeat n \#)))

(defn try-place [original idx  piece pad]
  (let [bnd (if pad (unchecked-inc piece) piece)
        c (char-at original idx)]
    (when (or (= c \?) (= c \#))
      ;;is there space?
      (loop [r   idx
             n   bnd]
        (println [r n (char-at original r)])
        (if (zero? n)
           (if pad
             (str (get-piece piece) pad)
             (get-piece piece))
          (let [c (char-at original r)]
            (if (and pad (= n 1))
              (when  (or (= c \.) (= c \?))
                (recur (unchecked-inc r) (unchecked-dec n)))
              (when (or (= c \#) (= c \?))
                (recur (unchecked-inc r) (unchecked-dec n))))))))))

(defn possible-bounds [original xs]
  (let [n (count original)
        pads (dec (count xs))
        used (reduce + pads xs)]
    {:length n
     :slack (- n used)
     :used   used
     :widths (conj (mapv inc (butlast xs)) (peek xs))}))

;;slack determines how much drift we have.  there are a constant number of dots
;;between (the slack).

(defn construct [original xs]
  (let [{:keys [slack widths]} (possible-bounds xs)
        n                      (count widths)]
    (loop [pending (list {:idx 0 :slack slack :n (count widths)
                          :current ""})
           acc  []]
      (if-let [{:keys [idx slack n current] :as state} (first pending)]
        (let [;;generate neighbors from slack.
              nebs (for [i (range 0 slack)]
                     (let [placed (try-place original  )])
                     )
              ]
        )
        acc))))


;;naive combinations.
(defn combos [original xs]
  (let [n    (count original)
        pads (dec (count xs))
        used (reduce + pads xs)
        pats (clojure.string/join \.
               (map (fn [n] (apply str (repeat n \#))) xs))]
    (str pats (apply str (repeat (- n used) \.)))))

;;add a dot to the left/right of each, and their children, and on,
;;until we hit a length.
;;report result.
(defn generate [length n]

  )

;;"###.##.#...."
;;{:length 12
;; :evens [0 1 1 4]
;; :odds  [3 2 1]
;; :slack 4}

;;init
;;{:length 12
;; :evens }

(for [i (range 0 (inc 4))
      j (range 1 (inc 4))
      k (range 1 (inc 4))
      l (range 0 (inc 4))
      :when (= (+ i j k l) 6)]
  [i j k l])


;;for testing
(defn dots [n]
  (apply str (repeat  n \.)))

(defn pounds [n]
  (apply str (repeat  n \#)))

(defn gen [{:keys [length odds] :as state }  bound evens slack n current]
  (if (= n bound) ;;done
    [current]
    (let [internal? (< 0 n (dec bound))
          lb (cond (= n (dec bound))  slack ;;have to distribute remainder.
                   internal? 1 ;;internal, have to distribute 1.
                   :else               0)]
      (->> (for [i (range lb (+ slack (if internal? 2 1)))] ;;internal gets 1 for free.
             (let [enxt (conj evens i)
                   snxt (if internal?
                          (- slack (dec i)) ;;padded already added.
                          (- slack i))
                   dots (dots i)
                   new-string (if (< n (dec bound))
                                (str current dots (pounds (odds n)))
                                (str current dots))
                   _ (println [n i enxt snxt new-string])]
               (gen state bound enxt snxt (inc n) new-string)))
           (apply concat)))))
