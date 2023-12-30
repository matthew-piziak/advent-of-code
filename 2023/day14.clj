(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.core.matrix :as mat]
            [aoc.grid :as g]))

(defn tilt [s col]
  (->> col (partition-by #(= \# %)) (map s) flatten (into [])))

(defn tilt-north [m]
  (->> m mat/transpose (map (partial tilt (fn [c] (reverse (sort c))))) mat/transpose))

(defn tilt-west [m]
  (->> m (map (partial tilt (fn [c] (reverse (sort c)))))))

(defn tilt-south [m]
  (->> m mat/transpose (map (partial tilt (fn [c] (sort c)))) mat/transpose))

(defn tilt-east [m]
  (->> m (map (partial tilt (fn [c] (sort c))))))

(defn score [m]
  (let [stone-score (fn [i x] (if (= x \O) (- (mat/column-count m) i) 0))]
    (apply + (map (comp (partial apply +) (partial map-indexed stone-score)) (mat/transpose m)))))

;;; ex 14-1: 136
(->> "example14" u/read-lines (map seq) mat/matrix tilt-north score)

;;; ans 14-1: 108144
(->> "input14" u/read-lines (map seq) mat/matrix tilt-north score)

(defn spin [m]
  (let [spin (fn [m] (->> m tilt-north tilt-west tilt-south tilt-east))]
    (loop [i 0 spun m seen {}]
      (let [h (hash spun)]
        (if (seen h) (nth (iterate spin spun) (mod (- 1000000000 (seen h)) (- i (seen h))))
            (recur (inc i) (spin spun) (conj seen [h i])))))))

;;; ex 14-2: 64
(->> "example14" u/read-lines (map seq) mat/matrix spin score)

;;; ans 14-2: 108404
(->> "input14" u/read-lines (map seq) mat/matrix spin score)
