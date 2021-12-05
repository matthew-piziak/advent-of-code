(ns aoc.day05
    (:require [aoc.utils :as u]))

(def input (u/read-lines "input05"))

(defn seat->row [seat]
  (Integer/parseInt (apply str (replace {\F 0 \B 1} (take 7 seat))) 2))

(defn seat->col [seat]
  (Integer/parseInt (apply str (replace {\L 0 \R 1} (take-last 3 seat))) 2))

(defn seat->id [seat]
  (+ (* (seat->row seat) 8) (seat->col seat)))

;;; ans 05-1: 813
(apply max (map seat->id input))

;;; ans 05-2: 612
(->> input
   (map seat->id)

   ;; find the gap
   sort
   (map (fn [m t] [m t (== m t)]) (range 6 814))
   (filter (fn [[_ _ e]] (not e)))

   ;; extract the answer
   first
   first)
