(ns aoc.day12
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn parse [in]
  (let [edges (reduce (fn [m [a b]] (merge-with set/union m {a #{b} b #{a}})) {} (map #(s/split % #"-") in))]
    (into {} (for [[k v] edges] [k (disj v "start")]))))

(defnp small-cave? [s]
  (every? #(Character/isLowerCase %) s))

(defn paths [curr edges path]
  (if (= curr "end") path
      (let [next-edges (if (small-cave? curr) (into {} (for [[k v] edges] [k (disj v curr)])) edges)]
        (flatten (for [next (edges curr)] (paths next next-edges (str path "," next)))))))

(defn count-paths [in]
  (count (paths "start" (parse in) "start")))

(def small (u/read-lines "small12"))

;;; 10
(count-paths small)

(def example (u/read-lines "example12"))

;;; ex12-1: 19
(count-paths example)

(def large (u/read-lines "large12"))

;;; 226
(count-paths large)

(def input (u/read-lines "input12"))

;;; ans12-1: 4549
(count-paths input)

(defnp invalid? [path]
  (let [freqs (filter (fn [[k v]] (and (small-cave? k) (>= v 2))) (frequencies (s/split path #",")))]
    (or (some (partial < 2) (vals freqs)) (< 1 (count freqs)))))

(defn filter-small-caves [nexts small-caves]
  (let [vs (set (vals small-caves))
        ks (set (keys small-caves))]
    (if (vs 2) (set/difference nexts ks) nexts)))

(defnp paths2
  ([edges] (paths2 edges "start" "start" {}))
  ([edges curr path small-caves]
   (cond
     (= curr "end") 1
     :else
     (let [next-small-caves (if (small-cave? curr) (merge-with + small-caves {curr 1}) small-caves)]
       (apply + (map #(paths2 edges % (str path "," %) next-small-caves) (filter-small-caves (edges curr) small-caves)))))))

;;; 36
(-> small parse paths2)

;;; ex12-2: 103
(-> example parse paths2)

;;; 3509
(-> large parse paths2)

;;; ans12-2: 120535
(-> input parse paths2)
