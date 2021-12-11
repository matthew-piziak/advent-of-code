(ns aoc.day11
  (:require [clojure.core.matrix :as mat]
            [clojure.set :as set]
            [aoc.utils :as u]))

(import 'java.awt.image.BufferedImage 'javax.imageio.ImageIO 'java.awt.Color 'java.io.File)

(def small
  (mat/matrix [[1 1 1 1 1]
               [1 9 9 9 1]
               [1 9 1 9 1]
               [1 9 9 9 1]
               [1 1 1 1 1]]))

(def example [[5 4 8 3 1 4 3 2 2 3]
              [2 7 4 5 8 5 4 7 1 1]
              [5 2 6 4 5 5 6 1 7 3]
              [6 1 4 1 3 3 6 1 4 6]
              [6 3 5 7 3 8 5 4 7 8]
              [4 1 6 7 5 2 4 6 4 5]
              [2 1 7 6 8 4 1 7 2 1]
              [6 8 8 2 8 8 1 1 3 4]
              [4 8 4 6 8 4 8 5 5 4]
              [5 2 8 3 7 5 1 5 2 6]])

(defn render [octopuses]
  (let [[l h] (mat/shape octopuses)
        block (/ 200 (max l h))
        bi (BufferedImage. (* block l) (* block h) BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)]
    (do
      (doseq [[x y] (mat/index-seq octopuses)]
        (let [energy-color (int (* 255 (* 0.09 (get-in octopuses [x y]))))]
          (.setColor g (java.awt.Color. energy-color energy-color energy-color))
          (.fillRect g (* block x) (* block y) block block)))
      bi)))

(defn energize [octopuses]
  (mat/emap inc octopuses))

(defn flashing? [grid idx]
  (> (get-in grid idx) 9))

(defn flashing-idxs [grid]
  (set (filter (partial flashing? grid) (mat/index-seq grid))))

(defn in-bounds? [[l h] [x y]]
  (and (>= x 0) (>= y 0) (< x l) (< y h)))

(defn adj [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (= [dx dy] [0 0]))]
    [(+ x dx) (+ y dy)]))

(defn neighborhood [grid [x y]]
  (let [[l h] (mat/shape grid)]
    (filter (partial in-bounds? [l h]) (adj [x y]))))

(defn flash [grid flashing-idxs]
  (reduce #(update-in %1 %2 inc) grid (mapcat (partial neighborhood grid) flashing-idxs)))

(defn flashing-cascade [grid flashing-frontier flashing-total]
  (let [new-grid (flash grid flashing-frontier)
        new-flashing (set/difference (flashing-idxs new-grid) flashing-total)
        new-total (set/union new-flashing flashing-total)]
    (if (empty? new-flashing) [new-grid new-total]
        (recur new-grid new-flashing new-total))))

(defn step [grid]
  (let [energized (energize grid)
        [flashed new-flashes] (flashing-cascade energized #{} #{})]
    [(reduce #(assoc-in %1 %2 0) flashed (flashing-idxs flashed)) (count new-flashes)]))

(defn steps
  ([grid n] (steps grid n 0))
  ([grid n flashes]
   (if (zero? n) flashes
       (let [[new-grid new-flashes] (step grid)]
         (recur new-grid (dec n) (+ flashes new-flashes))))))

;;; ex11-1: 1656
(steps example 100)

(def input (mat/matrix (mapv #(mapv (fn [n] (Integer/parseInt (str n))) %) (mapv seq (u/read-lines "input11")))))

;;; ans11-1: 1743
(steps input 100)

(defn sync-up
  ([grid] (sync-up grid 0))
  ([grid n]
   (if (mat/zero-matrix? grid) n
       (let [[new-grid _] (step grid)]
         (recur new-grid (inc n))))))

;;; 6
(sync-up small)

;;; ex11-2: 195
(sync-up example)

;;; ans11-2: 364
(sync-up input)
