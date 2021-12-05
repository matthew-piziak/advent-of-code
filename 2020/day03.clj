(ns aoc.day03
    (:require [aoc.utils :as u]))

(def input (u/read-lines "input03"))

(defn slope-encounters [s1 s2]
  (frequencies (map #(nth (nth input %1) (mod %2 31)) (range 0 323 s1) (range 0 10000 s2))))

(defn trees [fs]
  (get fs \#))

;;; ans-03-1: 198
(trees (slope-encounters 1 3))

;;; ans-03-2: 5140884672
(* (trees (slope-encounters 1 3))
   (trees (slope-encounters 1 1))
   (trees (slope-encounters 1 5))
   (trees (slope-encounters 1 7))
   (trees (slope-encounters 2 1)))
