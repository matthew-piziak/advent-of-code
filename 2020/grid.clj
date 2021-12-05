(ns aoc.grid
  (:require [aoc.utils :as u]))

(defn atxy [grid [x y]]
  (nth (nth grid x "") y \.))

(defn- moore-neighborhood [[x y] d]
  (for [dx [(- d) 0 d]
        dy [(- d) 0 d]
        :when (not (= [dx dy] [0 0]))]
    [(+ x dx) (+ y dy)]))

(defn neighborhood [grid [x y] d]
  (map (partial atxy grid) (moore-neighborhood [x y] d)))

(defn- size [grid]
  [(count (first grid)) (count grid)])

(defn coords [grid]
  (let [[xs ys] (size grid)]
    (for [x (range 0 xs) y (range 0 ys)] [x y])))

(defn render [grid]
  (u/unlines (map (partial apply str) grid)))

(defn eq? [g1 g2]
  (= (map (partial apply str) g1) (map (partial apply str) g2)))
