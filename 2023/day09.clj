(ns aoc.day09
  (:require [aoc.utils :as u]))

(defn ds [xs]
  (letfn [(d [xs] (map (fn [[x hx]] (- hx x)) (partition 2 1 xs)))]
    (reductions #(if (every? zero? %1) (reduced %1) %2) (iterate d xs))))

(defn line->ds [l]
  (->> l (re-seq #"-?\d+") (map #(Integer/parseInt %)) ds))

(defn next-history [ds]
  (transduce (map last) + 0 ds))

;;; exs 09-1: 114,
(u/sum-lines "example09" (map (comp next-history line->ds)))

;;; ans 09-2: 1980437560
(u/sum-lines "input09" (map (comp next-history line->ds)))

(defn pre-history [ds]
  (reduce (fn [x hx] (- hx x)) (reverse (map first ds))))

;;; ex 09-2: 2
(u/sum-lines "example09" (map line->pre-history))

;;; ans 09-2: 977
(u/sum-lines "input09" (map line->pre-history))
