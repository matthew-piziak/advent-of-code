(ns aoc.day01
    (:require
     [aoc.utils :as u]
     [clojure.math.combinatorics :as combo]))

(defn read-input-01 []
  (map #(Integer/parseInt %) (u/read-lines "input01")))

(defn prod-of-first-match [pred? combos]
  (apply * (first (filter pred? combos))))

;;; ans01-1: 365619
(let [pairs (combo/combinations (read-input-01) 2)]
  (letfn [[sums-to-2020? [[n m]] (== (+ n m) 2020)]]
    (prod-of-first-match sums-to-2020? pairs)))

;;; ans01-2: 236873508
(let [trips (combo/combinations (read-input-01) 3)]
  (letfn [[sums-to-2020? [[n m l]] (== (+ n m l) 2020)]]
    (prod-of-first-match sums-to-2020? trips)))
