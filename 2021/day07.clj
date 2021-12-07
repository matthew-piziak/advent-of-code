(ns aoc.day07
  (:require [aoc.utils :as u]
            [fastmath.stats :as stats]))


(def example [16 1 2 0 4 2 7 1 2 14])

(defn dist [p n]
  (Math/abs (- p n)))

(defn fuel [in]
  ;; the median minimizes error
  (let [m (stats/median in)]
    (int (apply + (map (partial dist m) in)))))

;;; ex07-1: 37
(fuel example)

(def input (map #(Long/parseLong %) (re-seq #"\d+" (u/read-resource "input07"))))

;;; ans07-1: 356958
(fuel input)

(defn tri-n [n]
  (if (zero? n) 0
      (apply + (range 1 (inc n)))))

(defn hrange [in]
  ((juxt (partial apply min) (partial apply max)) in))

(defn fuel2 [in]
  ;; the mean minimizes square error, but check both ceil and floor because triangle numbers are not quite squaring
  (let [[mf mc] ((juxt #(Math/floor %) #(Math/ceil %)) (stats/mean in))]
    (min
     (apply + (map #(tri-n (dist mf %)) in))
     (apply + (map #(tri-n (dist mc %)) in)))))

;;; ex07-2: 168
(fuel2 example)

;; ans07-2: 105461913
(fuel2 input)
