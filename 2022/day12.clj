(ns aoc.day12
    (:require [aoc.utils :as u]
              [clojure.core.matrix :as mat]
              [clojure.string :as s]
              [astar.core :as astar]))

(def example (mat/matrix (map seq (u/read-lines "example12"))))

(defn atxy [grid [x y]]
  (get-in grid [x y]))

(defn neighbors [m [x y]]
  ;; up, down, left, right
  (let [h (int (atxy m [x y]))]
    (filter
     (complement nil?)
     (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
       (let [n [(+ x dx) (+ y dy)]
             nh (int (or (atxy m n) 9999))]
         (when (and nh (<= nh (inc h))) n))))))

(defn h [grid [gx gy] [x y]]
  (+ (Math/abs (- gx x)) (Math/abs (- gy y))))

(defn normalize-start-and-end [m]
  (mat/emap (fn [c] (case c \S \a \E \z c)) m))

(defn find-idx [m c]
  (for [x (range (mat/dimension-count m 0))
        y (range (mat/dimension-count m 1))
        :when (= (atxy m [x y]) c)]
    [x y]))

(defn path [in start]
  (let [graph (fn [[x y]] (neighbors (normalize-start-and-end in) [x y]))
        dist (fn [_d1 _d2] 1)
        hh (fn [[x y]] (h (normalize-start-and-end in) (first (find-idx in \E)) [x y]))
        goal (first (find-idx in \E))]
    (count (astar/route graph dist hh start goal))))

;;; ex 12-1: 31
(path example (first (find-idx example \S)))

(def input (mat/matrix (map seq (u/read-lines "input12"))))

;;; ans 12-1: 449
(path input (first (find-idx input \S)))

(defn min-path [in]
  (apply min (map (partial path in) (find-idx (normalize-start-and-end in) \a))))

;;; ex 12-2: 29
(min-path example)

;;; ans 12-2: 443
(min-path input)
