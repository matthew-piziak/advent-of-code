(ns aoc.day01
  (:require [aoc.utils :as u]))

(def example (u/read-ints "example01"))

(defn scan [last scans increases]
  (if (empty? scans) increases
      (let [new (first scans)]
        (recur new (rest scans) (if (> new last) (inc increases) increases)))))

(scan (first example) (rest example) 0)

(def input (u/read-ints "input01"))

;;; ans 01-1 1288
(scan (first input) (rest input) 0)

(defn scanw [input]
  (let [windows (map (partial apply +) (partition 3 1 input))]
    (scan (first windows) (rest windows) 0)))

;;; ans 01-2 1311
(scanw input)
