(ns aoc.day11
  (:require [aoc.utils :as u]
            [aoc.grid :as g]
            [clojure.math.combinatorics :as combo]))

(defn empties [input]
  (let [[wx wy] (g/grid-size input)
        grid (g/read-grid input)
        empty-rows (loop [x 0 y 0 empties []]
                     (cond
                       (> y wy) empties
                       (= (grid [x y]) \#) (recur 0 (inc y) empties)
                       (> x wx) (recur 0 (inc y) (conj empties y))
                       :else (recur (inc x) y empties)))
        empty-cols (loop [x 0 y 0 empties []]
                     (cond
                       (> x wx) empties
                       (= (grid [x y]) \#) (recur (inc x) 0 empties)
                       (> y wy) (recur (inc x) 0 (conj empties x))
                       :else (recur x (inc y) empties)))]
    [empty-rows empty-cols]))

(defn dist [[empty-rows empty-cols] ef [[x y] [xx yy]]]
  (letfn [(between [[x y] r] (or (< x r y) (> x r y)))]
    (let [nr (count (filter (fn [r] (between [y yy] r)) empty-rows))
          nc (count (filter (fn [c] (between [x xx] c)) empty-cols))]
      (+ (Math/abs (- x xx)) (Math/abs (- y yy)) (* (+ nr nc) ef)))))

(defn tot-dist [input ef]
  (let [galaxies (keys (filter (fn [[_ c]] (= c \#)) (g/read-grid input)))
        pairs (map (partial into []) (combo/combinations galaxies 2))
        es (empties input)]
    (transduce (map #(dist es ef %)) + 0 pairs)))

;;; ex 11-1: 374
(tot-dist "example11" 1)

;;; ans 11-1: 9684228
(tot-dist "input11" 1)

;;; exs 11-2: 1030, 8410
(tot-dist "example11" 9)
(tot-dist "example11" 99)

;;; ans 11-2: 483844716556
(tot-dist "input11" 999999)
