(ns aoc.day10
  (:require [aoc.utils :as u]))

(def example (u/read-ints "example10"))
(def medium (u/read-ints "medium10"))
(def input (u/read-ints "input10"))

(defn device-joltage [ins]
  (+ 3 (apply max ins)))

(defn sort-with-ends [ins]
  (sort (conj ins 0 (device-joltage ins))))

(defn diffs [ins]
  (map (fn [[n m]] (- m n)) (partition 2 1 (sort-with-ends ins))))

;;; ans 10-1: 2760
(->> input
   diffs
   frequencies
   vals
   (apply *))

(defn ways [n]
  (case n
    1 1
    2 2
    3 4
    (apply + (map ways [(- n 1) (- n 2) (- n 3)]))))

;; ans 10-2: 13816758796288
(->> input
    diffs
    (u/split-by #(= 3 %))
    (map (comp ways count))
    (apply *))
