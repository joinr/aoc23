(ns aoc23.day15
  (:require [aoc23.util :as u]
            [clojure.string :as s]))

(defn HASH [^String s]
  (reduce (fn [acc c]
            (-> acc
                (+ (int c))
                (* 17)
                (mod 256))) 0 s))

(def sample
  "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn parse-input [txt]
  (-> txt s/trim (s/split  #",")))
(defn solve1 []
  (->> (u/slurp-resource "day15.txt")
       parse-input
       (map HASH)
       (reduce +)))


;;part 2


(defn parse-input2 [txt]
  (->>  (-> txt s/trim (s/split #","))
        (map (fn [x]
               (let [idx (or (s/index-of x \=)
                             (s/index-of x \-))
                     [l r] [(subs x 0 idx) (subs x idx)]
                     op  (keyword (subs r 0 1))
                     ins {:label l
                          :box (HASH l)
                          :op op}]
                 (if (= op :=)
                   (assoc ins :n (parse-long (subs r 1)))
                   ins))))))

;;have 0-255 boxes.
;;lenses are stored in boxes by order of insertion (fifo).
;;lenses are labeled from the instruction.
;;hash maps to the box to insert/remove.

;;then we can maintain unique entries.
;;[[l1 0] [l2 1] [l3 2]] {l1 [0 9] l2 [1 8] l3 [2 7]}
;;pop l2
;;[[l1 0] [l3 2]] {l1 [0 9] l3 [2 7]} ;;sorted by insertion order still remains.

;;update l2 -> 5
;;[[l1 0] [l2 1] [l3 2]] {l1 [0 9] l2 [1 5] l3 [2 7]}


;;they can also probably exist in multiple boxes.
;;can probably map lens->boxes with a byte or bitset.

;;possible to change lens of same label with new value.
(defn put-lens [{:keys [lenses lbl->info k] :as box} label n]
  (if-let [[k0 length] (lbl->info label)]
    (if (= length n)
      box ;;do nothing
      ;;update length registry
      (-> box  (update :lbl->info assoc label [k0 n])))
    ;;new lense
    (let [k1 (inc k)]
      {:k k1
       :lenses (conj lenses [label k1])
       :lbl->info (assoc lbl->info label [k1 n])})))

(defn remove-lens [{:keys [lenses lbl->info] :as box} lbl]
  (if-let [[k0 length] (lbl->info lbl)]
    ;;known, remove
    (-> box
        (update  :lenses disj [lbl k0])
        (update  :lbl->info dissoc lbl))
    box))

(defn ->box []
  {:k 0
   :lenses (sorted-set-by (fn [l r] (compare (l 1) (r 1))))
   :lbl->info {}})

(defn execute [boxes {:keys [label box op n] :as ins}]
  (update boxes box (case op
                      := #(put-lens    (or % (->box)) label n)
                      :- #(when % (remove-lens % label))
                      (throw (ex-info "bad op" {:in ins})))))


(defn power [box {:keys [lenses lbl->info]}]
  (let [bn (inc box)]
    (->> lenses
         (map-indexed (fn [idx kv]
                        (let [lbl (kv 0)
                              info (lbl->info lbl)
                              length (info 1)]
                          (* (inc idx) bn length)))))))

(defn solve2 [txt]
  (->> txt
       parse-input2
       (reduce execute {})
       (mapcat (fn [[box contents]]
                 (power box contents)))
       (reduce +)))

#_
(solve2 (u/slurp-resource "day15.txt"))
