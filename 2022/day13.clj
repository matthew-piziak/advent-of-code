(ns aoc.day13
    (:require [aoc.utils :as u]))

(def example (map (partial map read-string) (u/read-paras "example13")))

(defn compare-packets [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (number? a) (recur [a] b)
    (number? b) (recur a [b])
    :lists (or (first (drop-while zero? (map compare-packets a b)))
               (- (count a) (count b)))))

(defn sum-of-ordered-pair-idxs [in]
  (apply + (keep-indexed #(when (neg? (apply compare-packets %2)) (inc %1)) in)))

;;; ex 13-1: 13
(sum-of-ordered-pair-idxs example)

(def input (map (partial map read-string) (u/read-paras "input13")))

;;; ans 13-1: 5557
(sum-of-ordered-pair-idxs input)

(defn sort-with-dividers [in]
  (into [] (sort-by identity compare-packets (concat (apply concat in) [[[2]] [[6]]]))))

(defn at [in divider]
  (inc (.indexOf in divider)))

(defn prod-divider-idxs [in]
  (* (at in [[2]]) (at in [[6]])))

;;; ex 13-1: 140
(prod-divider-idxs (sort-with-dividers example))

;;; ans 13-2: 22425
(prod-divider-idxs (sort-with-dividers input))
