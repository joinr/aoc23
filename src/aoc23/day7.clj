(ns aoc23.day7
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


(def sample
"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(def default-char-order
  (let [cs "AKQJT98765432"]
    (zipmap (reverse cs) (range 1 (inc (count cs))))))

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map (fn [x]
              (let [[hand bid] (-> x (s/split #" "))]
                {:raw hand :hand (vec hand) :bid (parse-long bid)}
                )))))

#_
(defn compare-order [l r]
  (compare (order l) (order r)))

(defn compare-orders
  ([char-order idx bnd ls rs]
   (if (< idx bnd)
     (let [res (compare (-> idx ls char-order) (-> idx rs char-order))]
       (if-not (zero? res)
         res
         (recur char-order (unchecked-inc idx) bnd ls rs)))
     0))
  ([char-order ls rs] (compare-orders char-order 0 (count ls) ls rs))
  ([ls rs] (compare-orders default-char-order ls rs)))

;;array-map preserves order...so long as we don't mess with it.
(defn sorted-counts-desc [xs]
  (->> (frequencies xs)
       (sort-by #(- (val %)))
       (apply concat)
       (apply array-map)))

(def order (zipmap [:high :one :two :three :full :four :five]
                   (range 7)))

(defn classify [{:keys [hand] :as m}]
  (let [counts (sorted-counts-desc hand)
        most (val (first counts))]
    (assoc m :counts counts
           :type
           (case most
             5 :five
             4 :four
             3 (case (val (second counts))
                 2  :full
                 1  :three)
             2 (case (val (second counts))
            2 :two
            1 :one)
             :high)
           ;;appended for pt2.
           :wild (counts \J 0))))

(def fours (mapv vec ["33332" "2AAAA"]))
(def fulls (mapv vec ["77888" "77788"]))

(defn descend [f] (fn [l r] (f r l)))
;;compare classified hands.

(defn strength
  ([char-order l r]
   (let [res (compare (-> l :type order) (-> r :type order))]
     (if (not (zero? res)) res
         (compare-orders char-order (or (l :old) (l :hand)) (or (r :old) (r :hand))))))
  ([l r] (strength default-char-order l r)))

(defn solve1 [txt]
  (->> txt
       parse-input
       (map classify)
       (sort strength)
       (map-indexed (fn [idx m]
                      (assoc m :rank (inc idx))))
       (map (fn [{:keys [bid rank]}] (* bid rank)))
       (reduce +)))


;;jokers are present in the input.
;;so they matter now.
;;they can act as any card that will improve the hand.
;;give any hand, define transition to add joker to that hand.
;;distribute jokers to improve hand maximally.
;;sounds like dp.

;;alt distribute jokers over the space of cards present in the hand
;;to improve hand?

;;it' always better to minimize discrepancies.
;;maximize single cardinality, then minimize discrepancy.
;;so like JJJJA -> J's become A to make highest rank.
;;treat it as  hand we are growing.
;;what card improves the hand most?

;;strongest possible hand means not only improving the tuple, but improving
;;the tie breaking order (lexicographic sort order).
;;Looks like the only place we have that choice is for high card hands.
;;in that case, we can choose any value for the J to create a pair,
;;so we should choose the highest card in the set known set.

;;0 -> create pair from highest known
;;1234J -> 12344

;;1 pair -> three from the pair.
;;1224J -> 12242

;;2 pair -> full house, choose the larger of the pairs to improve strength.
;;1122J -> 11222
;;is there a case where we pick a lower value?  hmmm.

;;3 -> 4 from the triple.
;;1112J -> 11121

;;full-> Only possible if Js are both unknown, make a 5
;;111JJ -> 11111

;;5 -> Make highest value, A.
;;JJJJJ -> AAAAA

;;in any case where the jokers are the max, we need to add them to the
;;next value ordered by max count and then by char-order.

(def wild-char-order
  (let [cs "AKQT98765432J"]
    (zipmap (reverse cs) (range 1 (inc (count cs))))))

(defn next-best-key [counts]
  (let [mx (-> counts (dissoc \J) vals first)]
    (->> counts
         (drop-while (fn [kv] (= (key kv) \J)))
         (take-while (fn [kv] (= (val kv) mx)))
         (sort-by (fn [kv] [(- (val kv)) (- (wild-char-order (key kv)))]))
         first
         key)))

;;for purposes of ranking, we classify under new hand.
;;for ordering, we use old hand with wild-char-order.
(defn improve [{:keys [raw wild hand counts type] :as m}]
  (let [distribute (fn [newhand]
                     (let [r (assoc m :hand newhand :old hand :raw raw :old-type type)]
                       (classify r)))]
  (if (pos? wild)
    (case wild
      5 (distribute (vec "AAAAA"))
      (let [other (next-best-key counts)]
        (distribute (vec (replace {\J other } hand)))))
    m)))

(defn solve2 [txt]
  (->> txt
       parse-input
       (map (comp improve classify))
       (sort (fn [l r] (strength wild-char-order l r)))
       (map-indexed (fn [idx m]
                      (assoc m :rank (inc idx))))
       (map (fn [{:keys [bid rank]}] (* bid rank)))
       (reduce +)))
