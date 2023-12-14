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


(defn parse-line [ln]
  (let [[l r]  (s/split ln #" ")
        chunks (u/read-as-vector r)
        length (count l)
        hashes  (->> chunks (reduce +))
        pads    (dec (count chunks))
        used    (+ hashes pads)]
    {:original l
     :chunks chunks
     :length length
     :dots   (- length hashes)}))

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map parse-line)))

;;"###.##.#...."
;;{:length 12
;; :evens [0 1 1 4]
;; :odds  [3 2 1]
;; :slack 4}

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


(defn possible? [^String original  ^long idx ^long pounds ^long dots ]
  (let [bound (count original)]
    (if (> (+ pounds dots) bound)
      false
      (loop [idx idx
             dots dots
             pounds pounds]
        (cond (= idx bound)
              (and (zero? pounds) (zero? dots))
              (pos? pounds)
              (let [c (char-at original idx)]
                (if (or (= c \#) (= c \?))
                  (recur (unchecked-inc idx)
                         dots
                         (unchecked-dec pounds))
                  false))
              (pos? dots)
              (let [c (char-at original idx)]
                (if (or (= c \.) (= c \?))
                  (recur (unchecked-inc idx)
                         (unchecked-dec dots)
                         pounds)
                  false))
              :else          true)))))

(defn gen
  [s dots chunks idx]
   (if (= s "")
     (if (or (neg? dots) (seq chunks))
       0
       1)
     (case (char-at s 0)
       \. (if (pos? dots)
            (gen (subs s 1) (dec dots) chunks (inc idx))
            0)
       \# (if-let [n (nth chunks 0 nil)]
            (let [added-dot (if (= (count chunks) 1) 0 1)
                  size      (+ n added-dot)]
              (if (possible? s 0 n added-dot)
                (gen (subs s size) (- dots added-dot) (subvec chunks 1)
                     (+ idx size))
                0))
            0)
       \? (let [l (gen (str \. (subs s 1)) dots chunks idx)
                r (gen (str \# (subs s 1)) dots chunks idx)]
            (+ l r)))))

(defn gen-cached
  ([cache s dots chunks idx]
   (let [store (fn [k v]
                 (.put ^java.util.Map cache k v)
                 v)
         aux (fn aux [s dots chunks idx]
               (let [k [s dots chunks idx]]
                 (or (get cache k)
                   (store k
                          (if (= s "")
                            (if (or (neg? dots) (seq chunks))
                              0
                              1)
                            (case (char-at s 0)
                              \. (if (pos? dots)
                                   (aux (subs s 1) (dec dots) chunks (inc idx))
                                   0)
                              \# (if-let [n (nth chunks 0 nil)]
                                   (let [added-dot (if (= (count chunks) 1) 0 1)
                                         size      (+ n added-dot)]
                                     (if (possible? s 0 n added-dot)
                                       (aux (subs s size) (- dots added-dot) (subvec chunks 1)
                                            (+ idx size))
                                       0))
                                   0)
                              \? (let [l (aux (str \. (subs s 1)) dots chunks idx)
                                       r (aux (str \# (subs s 1)) dots chunks idx)]
                                   (+ l r))))))))]
     (aux s dots chunks idx)))
  ([s dots chunks idx] (gen-cached (java.util.HashMap.) s dots chunks idx)))

(defn solve [{:keys [original chunks length dots] :as in}]
  (gen original dots chunks 0))
(defn solve-cached [{:keys [original chunks length dots] :as in}]
  (gen-cached original dots chunks 0))

(defn solve1 [txt]
  (->> txt
       s/split-lines
       (map parse-line)
       (map solve-cached)
       (reduce +)))

#_(solve1 (u/slurp-resource "day12.txt"))

;;part2

;;naive unfolding...
;;probably fails.

(defn unfold [ln n]
  (let [[l r] (s/split ln #" ")
        l (s/join "?" (repeat n l))
        r (s/join "," (repeat n r))]
    (str l " " r)))

(defn solve2 [txt]
   (->> txt
        s/split-lines
        (map #(unfold % 5))
        (map parse-line)
        (map solve-cached)
        (reduce +)))

;;obe
;; (defn chunk-neighbor [original {:keys [chunks dots idx total]}]
;;   (when-let [c (first chunks)]
;;     (let [added-dot (if (> (count chunks) 1)
;;                       1
;;                       0)
;;           size (+ c added-dot)]
;;       (when (and (<= size total) (<= added-dot dots)
;;                  (possible? original idx c added-dot ))
;;         [size  {:chunks (subvec chunks 1)
;;                 :dots   (- dots added-dot)
;;                 :idx    (+ idx size)
;;                 :total  (- total size)}]))))

;; (defn dot-neighbors [original {:keys [chunks dots total idx]}]
;;   (let [dot-bound  (- dots (dec (count chunks)))]
;;     (for [i (range 1 (inc dot-bound))
;;           :when (and (<= i  total)
;;                      (possible? original idx 0 i ))]
;;       [i {:chunks chunks
;;           :dots  (- dots i)
;;           :total (- total i)
;;           :idx   (+ idx i)}])))

;; (defn all-neighbors [original init]
;;   (into (if-let [chnk (chunk-neighbor original init)]
;;           [chnk]
;;           [])
;;         (dot-neighbors original init)))

#_#_
(def hard "?###???????? 3,2,1")
(def hard-txt "?###????????")



;; (defn first-path [res]
;;   (let [[from to] (res :found-path)]
;;     (let [xs (first (u/paths (res :spt) from to))]
;;       {:txt (render-path xs) :nodes xs})))

;; (defn all-paths [res]
;;   (let [[from to] (res :found-path)]
;;     (->> (u/paths (res :spt) from to)
;;          (map render-path))))

#_
(defn as-chunk [{:keys [original odds length]}]
  (let [used (reduce + odds)]
    {:chunks odds
     :dots  (- length used)
     :total length
     :idx 0}))

;; ;;now we iterate.
;; (defn bellman-ford [original  from & {:keys [make-fringe on-visit]
;;                                       :or {make-fringe u/q
;;                                            on-visit identity}}]
;;   (let [init {:spt  {from from}
;;               :dist {from 0}
;;               :v    {from 1}
;;               :fringe (make-fringe [0 from])}]
;;     (loop
;;         [{:keys [spt dist fringe v] :as state} init]
;;       (if-let [source (u/peek-fringe fringe)]
;;         (if (= (source :total) 0)
;;           (assoc state :found-path [from source])
;;           (let [_ (on-visit source)
;;                 _ (when (and (v source)
;;                              (not= source from))
;;                     (println [:visited2x! source])
;;                     (throw (ex-info "badvisit" {:in state})))
;;                 vnew (let [preds (spt source)
;;                            ;_ (println [:src source :preds preds :v v])
;;                            pred-val  (if (set? preds)
;;                                        (->> preds
;;                                                            (map v)
;;                                                            (reduce +))
;;                                                       (v preds))
;;                            ;               _ (println [:pred-val pred-val])
;;                            ]
;;                                      pred-val)
;;                               vnxt (assoc v source vnew)
;;                             ;  _ (println [:vnxt vnxt])
;;                               sinks      (all-neighbors original source)
;;                               next-state (reduce (fn [acc [w sink]]
;;                                                    (u/relax acc source sink w))
;;                                                  (update state :fringe u/pop-fringe)
;;                                                  sinks)]
;;                           (recur (assoc next-state :v vnxt))))
;;                       state))))

;; (defn render-path [xs]
;;   (->> xs
;;        (partition 2 1)
;;        (map (fn [[l r]]
;;               (let [dots (- (l :dots) (r :dots))]
;;                 (if (= (l :chunks) (r :chunks))
;;                   (dot-chars dots)
;;                   (str (pound-chars (-> l :chunks first))
;;                        (dot-chars dots))))))
;;        (apply str)))
