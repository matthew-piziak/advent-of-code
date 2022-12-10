(ns aoc.day08
    (:require [aoc.utils :as u]))

(defn ->grid [input]
  (mapv (comp (partial mapv #(Character/digit % 10)) seq) (u/read-lines input)))

(defn transpose [grid]
  (apply mapv vector grid))

(defn visible? [[t & ts]]
  (every? #(< % t) ts))

(defn sight-lines [grid tgrid y x]
  [(subvec (grid y) x)
   (into [] (rseq (subvec (grid y) 0 (inc x))))
   (subvec (tgrid x) y)
   (into [] (rseq (subvec (tgrid x) 0 (inc y))))])

(defn score-sight-lines [scoring-function grid]
  (let [tgrid (transpose grid)]
    (for [y (-> grid count range)
          x (-> tgrid count range)]
      (scoring-function (sight-lines grid tgrid y x)))))

(defn count-visible [grid]
  (->> grid (score-sight-lines (partial some visible?)) (filter true?) count))

;;; ex 08-1: 21
(-> "example08" ->grid count-visible)

;;; ans 08-1: 1779
(-> "input08" ->grid count-visible)

(defn viewing-distance [[t & ts]]
  (min (count ts) (inc (count (take-while #(< % t) ts)))))

(defn max-scenic-score [grid]
  (->> grid (score-sight-lines (comp (partial apply *) (partial mapv viewing-distance))) (apply max)))

;;; ex 08-2: 8
(-> "example08" ->grid max-scenic-score)

;;; ans 08-2: 172224
(-> "input08" ->grid max-scenic-score)
