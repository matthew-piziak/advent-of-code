(ns aoc.day09
  (:require [aoc.utils :as u]
            [clojure.core.matrix :as mat]
            [the-flood.core :as flood]))

(defn in->grid [in]
  (mat/matrix (mapv (fn [r] (mapv #(Long/parseLong (str %)) (seq r))) (u/read-lines in))))

(def example (in->grid "example09"))

(defn lin-low [lin]
  (let [padded (concat [10] (conj lin 10))]
    (for [[x y z] (partition 3 1 padded)]
      (if (and (< y x) (< y z)) (inc y) 0))))

(defn col-lows [grid]
  (mat/transpose (mat/matrix (map lin-low (mat/columns grid)))))

(defn row-lows [grid]
  (mat/matrix (map lin-low (mat/rows grid))))

(defn total-risk [grid]
  (apply + (flatten (mat/emap (fn [x y] (if (= x y) x 0)) (col-lows grid) (row-lows grid)))))

;;; ex09-1: 15
(total-risk example)

(def input (in->grid "input09"))

;;; ans09-1: 535
(total-risk input)

(defn in-bounds? [grid [x y]]
  (let [[l h] (mat/shape grid)]
    (and (>= x 0) (>= y 0) (< x l) (< y h))))

(defn idxs [grid]
  (let [[l h] (mat/shape grid)]
    (for [x (range l)
          y (range h)
          :when (in-bounds? grid [x y])]
      [x y])))

(idxs example)

(flood/flood-fill (mat/emap (fn [p] (if (not= p 9) 0 9)) example) [0 0] (gensym) nil)

(defn borders [grid]
  (mat/emap (fn [p] (if (not= p 9) 0 9)) grid))

(defn atxy [grid [x y]]
  ((grid x) y))

(defn flood [grid idx]
  (if (= 0 (atxy grid idx))
    (flood/flood-fill grid (reverse idx) (gensym) nil)
    grid))

(defn flood-basins
  ([grid] (flood-basins grid (idxs grid)))
  ([grid idxs]
   (if (empty? idxs) grid
       (let [idx (first idxs)]
         (println idx)
         (recur (flood grid idx) (rest idxs))))))

(defn basin-prod [grid]
  (apply * (take 3 (reverse (sort (vals (dissoc (frequencies (flatten (flood-basins (borders grid)))) 9)))))))

;;; ex09-1: 1134
(basin-prod example)

;;; ans09-1: 1122700
(basin-prod input)
