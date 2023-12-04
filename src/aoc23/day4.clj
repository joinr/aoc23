(ns aoc23.day4
  (:require [clojure.string :as s]
            [clojure.set :as cljset]
            [aoc23.util :as u]))


(def sample
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

;;we know real input has numbers from 1 99

(defn parse-line [ln]
    (let [[l r] (clojure.string/split ln #":")
          [wins losses] (clojure.string/split r #"\|")]
      [l (u/read-as-vector wins) (u/read-as-vector losses)]))

(defn parse-cards [txt]
  (->> txt
       s/split-lines
       (map parse-line)))

(defn score-game [[g w l]]
  (let [n (->> (cljset/intersection (set w) (set l)) count)]
    (if (pos? n)
      (Math/pow 2 (dec n))
      0)))

(defn solve1 []
  (->> (u/slurp-resource "day4.txt")
       parse-cards
       (map score-game)
       (reduce +)))

;;part 2
;;pretty easy.
;;scan the deck.
;;maintaining state:
;;{:total 0 :cards [card1 card2 card3 card4 card5] :count}


(defn score-copies [[g w l]]
  (->> (cljset/intersection (set w) (set l)) count))

;;increment count by 1 (always an original) plus (card 0).

;;score the current card.

;;score determines how many copies to add to the next n
;;cards.

;;score of 3 - > [1 1 1 0 0] copies to distribute.

;;reused implementation from lanternfish 2021....
(defn ->cycle [start end entries]
  {:start start :end end :entries entries :count (inc end)})

(defn shift-right [{:keys [start end entries] :as cyc}]
  (assoc  cyc :start   (if (< start end) (inc start) 0)))

;;each day, advance the offset on the
;;general population.
(defn idx->local [cyc idx]
  (let [start  (cyc :start)]
    (rem (+ idx start) (cyc :count))))

(defn cycle-idx [cyc idx]
  (if (<= idx (cyc :end))
    (idx->local cyc idx)
    idx))

(defn cycle-nth [cyc idx]
  ((cyc :entries) (cycle-idx cyc idx)))

(defn cycle-assoc [cyc idx v]
  (update cyc :entries #(assoc % (cycle-idx cyc idx) v)))

(defn cycle-update [cyc idx f & args]
  (update cyc :entries #(apply update % (cycle-idx cyc idx) f args)))


;;we want to add 1 to the next score entries.
(defn distribute-copies [score idx cards]
  (let [mult (cycle-nth cards 0) ;;how many copies accumulated here.
        res (reduce (fn [acc idx]
                      (cycle-update acc idx + mult)) cards (range 1 (inc score)))]
    res))

;;represent the cards in a cycle, or a ring vector.
;;add the accumulate copies for the current card
;;and the original.  Since we populate the
;;state with 1's, we already accounted for the
;;original.
(defn advance [{:keys [total cards idx bnd] :as acc} nxt]
  (let [score        (score-copies nxt)
        new-cards    (distribute-copies score idx cards)
        shifted-cards (-> new-cards shift-right (cycle-assoc bnd 1))]
    (assoc acc
           :idx   (inc idx)
           :total (+ total (cycle-nth cards 0))
           :cards shifted-cards)))

(defn count-cards [xs]
  (let [init  (first xs)
        slots (count (second (first xs)))
        size  (inc slots)]
    (->> xs
         (reduce advance {:idx -1
                          :total 0
                          :cards (->cycle 0 (dec size) (vec (repeat size 1)))
                          :bnd   (dec size)}))))

(defn solve2 []
  (->> (u/slurp-resource "day4.txt")
       parse-cards
       count-cards
       :total))
