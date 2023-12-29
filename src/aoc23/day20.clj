(ns aoc23.day20
  (:require [aoc23.util :as u]
            [clojure.string :as s]))


;; Flip-flop modules (prefix %) are either on or off; they are initially off. If a
;; flip-flop module receives a high pulse, it is ignored and nothing happens.
;; However, if a flip-flop module receives a low pulse, it flips between on and
;; off. If it was off, it turns on and sends a high pulse. If it was on, it turns
;; off and sends a low pulse.
(defn ->flip [id state] {:id id :state state :type :flip})
(defn flip-node [from node evt]
  (case evt
    :low (case (node :state)
           :off [(assoc node :state :on) :high]
           :on [(assoc node :state :off) :low])
    nil))

;;Conjunction modules (prefix &) remember the type of the most recent pulse
;;received from each of their connected input modules; they initially default to
;;remembering a low pulse for each input. When a pulse is received, the
;;conjunction module first updates its memory for that input.

;;Then, if it remembers high pulses for all inputs, it sends a low pulse; otherwise, it
;;sends a high pulse.
;;needs to have a threshold to transmit a low pulse.
(defn ->conjunc [id state] {:id id :state state :type :conj})
;;track how many are on.  if the threshold is met, change state
;;to low, else high.
(defn conjunc-node [from node evt]
  (let [on     (node :on)
        new-on (case evt :high
                      (conj on from)
                      (disj on from))]
    (let [new-node (assoc node :on new-on)]
      (if (= (count new-on) (node :trigger)) ;;meet the thresh.
        [(assoc new-node :state :low) :low]
        [(assoc new-node :state :high) :high]))
    #_
    (if  (identical? new-on on)
      [node (node :state)] ;;no change
      (let [new-node (assoc node :on new-on)]
        (if (= (count new-on) (node :trigger)) ;;meet the thresh.
          [(assoc new-node :state :low) :low]
          [(assoc new-node :state :high) :high])))))

;;could multimethod or protocol, but small hierarchy.  easy enough to dispatch
;;on field.
(defn handle [from node evt]
;  (println node)
  (case (node :type)
    :flip (flip-node from node evt)
    :conj (conjunc-node from node evt)
    :echo [nil evt]
    :output [(case evt
               :high (assoc node :high (inc (node :high 0)))
               :low (assoc node :low (inc (node :low 0)))) evt]))

(def sample1
"broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(def sample2
"broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")

;;should be 2000 low 1000 high.  getting 3k..
(def sample3
"broadcaster -> f
%f -> a, b
&a -> c
&b -> c
&c -> output")
;;should be 3500h, 4500l....I am off by 2.

(def sample4
"broadcaster -> a
&a -> output")

(defn parse-input [txt]
  (->> txt
       s/split-lines
       (map (fn [ln]
              (let [[source sinks] (s/split ln #" -> ")
                    sinks          (u/read-as-keys  sinks)]
                (case source
                  "broadcaster" [:broadcaster {:type :echo :sinks sinks}]
                  (let [c  (nth source 0)
                        id (keyword (subs source 1))]
                  (case (nth source 0)
                    \% [id (assoc (->flip id :off) :sinks sinks)]
                    \& [id (assoc (->conjunc id :low) :sinks sinks)]))))))
       (into {})))

(defn add-sources [net]
  (reduce-kv (fn [acc src {:keys [sinks]}]
               (reduce (fn [acc snk]
                         (update-in acc [snk :sources] (fn [xs]
                                                         (conj (or xs []) src))))
                       acc sinks)) net net))

(defn label-output [net]
  (reduce-kv (fn [acc id {:keys [sinks] :as nd}]
               (if-not sinks
                 (assoc acc id (assoc nd :type :output :sinks [] :high 0 :low 0))
                 acc)) net net))

(defn init-memory [net]
  (reduce-kv (fn [acc k {:keys [type sources] :as nd}]
               (if (= type :conj)
                 (assoc acc k (assoc nd :on #{} :trigger (count sources)))
                 acc)) net net))

(defn parse-net [txt] (->> txt parse-input add-sources label-output init-memory))

(def emptyq clojure.lang.PersistentQueue/EMPTY)

(defn broadcast
  ([net  remaining low hi]
   (if-let [nxt (peek remaining)]
     (let [from   (nxt 0)
           parent (nxt 1)
           evt    (nxt 2)
           nd     (net parent)
           #_#_
           _ (println [:handling nxt #_nd])
           lnew (if (= evt :low) (inc low) low)
           hnew (if (= evt :high) (inc hi) hi)]
       (if-let [change (handle from (net parent) evt)]
         (let [sinks  (nd :sinks)
               newnd  (change 0)
               newevt (change 1)
               #_#__ (println [:changed change])]
           (recur (if newnd (assoc net parent newnd) net)
                  (->> sinks
                       (mapv (fn [snk] [parent snk newevt]))
                       (into (pop remaining)))
                  lnew
                  hnew))
         (recur net (pop remaining) lnew hnew)))
     {:low low :high hi :net net}))
  ([net evt]
   (broadcast net (conj emptyq [:button :broadcaster evt]) 0 0)))

(defn push-n [net n]
  (reduce (fn [{:keys [low high net]} idx]
            #_(println [:PUSH>>>>>>> (inc idx) {:low low  :high high}])
            (let [res (-> net (broadcast :low))]
              {:low (+ low (res :low))
               :high (+ high (res :high))
               :net (res :net)}))
          {:low 0 :high 0 :net net} (range n)))

;;need to detect cycling. similar to the rock sliding game.
;;can hash the state.

(defn solve1 [txt]
  (let [{:keys [high low]}
        (-> txt
            parse-net
            (push-n 1000))]
    (* high low)))

