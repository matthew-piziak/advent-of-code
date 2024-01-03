(ns aoc.day17
  (:require [aoc.grid :as g]
            [clojure.data.priority-map :as q]
            [clojure.pprint :as pp]))

(defn read-city [in]
  (into {} (map (fn [[k v]] [k (Integer/parseInt (str v))]) (g/read-grid in))))

(def start (q/priority-map {:pos [0 0] :mom :mu} 0))

(defn roll-heats [city heat ps]
  (rest (reductions (fn [a p] (cond (nil? a) nil (not (city p)) nil :else (+ a (city p)))) heat ps)))

(defn adjs [city [mn mx] block]
  (let [[{:keys [pos mom]} heat] block
        [px py] pos
        rs (range 1 (inc mx))
        op (case mom :mu :mu :rt :lt :lt :rt :up :dn :dn :up)
        invalid? (fn [[k v]] (or (nil? v) (= (:mom k) mom) (= op (:mom k))))
        ->adjs (fn [ps m] (drop (dec mn) (map (fn [p h] [{:pos p :mom m} h]) ps (roll-heats city heat ps))))]
    (remove invalid?
            (concat
             (->adjs (map (fn [r] [(+ px r) py]) rs) :rt)
             (->adjs (map (fn [r] [(- px r) py]) rs) :lt)
             (->adjs (map (fn [r] [px (+ py r)]) rs) :dn)
             (->adjs (map (fn [r] [px (- py r)]) rs) :up)))))

(defn search
  ([input goal bounds] (search (read-city input) goal bounds {} start {}))
  ([city [lx ly] [mn mx] seen frontier prev] ; path can be debugged with prev's adjacency matrix
   (if (empty? frontier) (second (first (sort-by second (filter (fn [[k v]] (= (:pos k) [lx ly])) seen))))
       (let [[mk mv] (peek frontier)
             better (filter (fn [[k h]] (or (not (seen k)) (> (seen k) h))) (adjs city [mn mx] [mk mv]))
             nprev (reduce (fn [p [k h]] (assoc p [k h] [mk mv])) prev better)]
         (recur city [lx ly] [mn mx] (merge-with min seen (into {} better)) (into (pop frontier) better) nprev)))))

;;; ex 17-1: 102
(search "example17" [12 12] [1 3])

;;; ans 17-1: 886
(search "input17" [140 140] [1 3])

;;; exs 17-2: 94, 71
(search "example17" [12 12] [4 10])
(search "large17" [11 4] [4 10])

;;; ans 17-2: 1055
(search "input17" [140 140] [4 10])
