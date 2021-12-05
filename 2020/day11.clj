(ns aoc.day11
  (:require [aoc.utils :as u]
            [aoc.grid :as g]))

(def example-grid (map seq (u/read-lines "example11")))
(def input-grid (map seq (u/read-lines "input11")))

(defn step-1 [grid [x y]]
  (let [curr (g/atxy grid [x y])
        neighbors (frequencies (g/neighborhood grid [x y] 1))]
    (cond
      (= curr \.) \.
      (and (= curr \L) (= (get neighbors \# 0) 0)) \#
      (and (= curr \#) (>= (get neighbors \# 0) 4)) \L
      :else curr)))

(defn step-grid [step grid]
  (into [] (partition (count grid) (map (partial step grid) (g/coords grid)))))

(defn rep-iteration [idx grid]
  (println (format "iteration=%s grid=\n%s" idx (g/render grid))))

(defn count-grid-fix-occupied [grid step]
  (get (->> (iterate (partial step-grid step) grid)
          (u/seq-peek 1 rep-iteration)
          (partition 2)
          (filter (fn [[x y]] (g/eq? x y)))
          (first)
          (first)
          (map frequencies)
          (apply merge-with +)) \# 0))

;;; ans 11-1: 2476
(count-grid-fix-occupied example-grid step-1)

(defn transpose [xs]
  (apply map list xs))

(defn occ-before-empt [s]
  (cond
    (empty? s) false
    (= (first s) \#) true
    (= (first s) \L) false
    :else (occ-before-empt (rest s))))

(defn count-neighborhood-occupied [grid [x y]]
  (let [neighborhoods (map #(g/neighborhood grid [x y] %) (range 1 (count grid)))]
    (count (filter occ-before-empt (transpose neighborhoods)))))

(defn step-2 [grid [x y]]
  (let [curr (g/atxy grid [x y])
        occupied (count-neighborhood-occupied grid [x y])]
    (cond
      (= curr \.) \.
      (and (= curr \L) (= occupied 0)) \#
      (and (= curr \#) (>= occupied 5)) \L
      :else curr)))

;; ans 11-2: 2257
(count-grid-fix-occupied input-grid step-2)

(frequencies (apply str (u/read-lines "result11")))
