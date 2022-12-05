(ns aoc.day04
  (:require [aoc.utils :as u]
            [clojure.set :as set]))

(defn range-set [s e]
  (set (range s (inc e))))

(defn range-valid? [f [s1 e1 s2 e2]]
  (f (range-set s1 e1) (range-set s2 e2)))

(defn parse-lines [lines]
  (map (comp vec (partial map #(Integer/parseInt %)) (partial re-seq #"\d+")) lines))

(defn count-valid [f input]
  (->> input u/read-lines parse-lines (filter #(range-valid? f %)) count))

(defn count-contained [input]
  (count-valid #(or (set/subset? %1 %2) (set/subset? %2 %1)) input))

;;; ex 04-1: 2
(count-contained "example04")

;;; ans 04-1: 518
(count-contained "input04")

(defn count-overlapped [input]
  (count-valid #(not-empty (set/intersection %1 %2)) input))

;;; 4
(count-overlapped "example04")

;;; 909
(count-overlapped "input04")
