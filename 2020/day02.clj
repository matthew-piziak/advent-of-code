(ns aoc.day02
    (:require aoc.utils :as u))

(defn pass-valid? [pass valid?]
  (let [[[_ min max c p]] (re-seq #"(\d+)-(\d+) (\w): (\w*)" pass)]
    (valid? (Integer/parseInt min) (Integer/parseInt max) c p)))

(defn valid-1? [min max c p]
  (<= min (get (frequencies p) (first c) 0) max))

(defn pass-valid-1? [pass]
  (pass-valid? pass valid-1?))

;;; ans02-1: 447
(count (filter pass-valid-1? (u/read-lines "input02")))

(defn valid-2? [min max c p]
  (letfn [[at [l] (= (first c) (nth p (- l 1)))]]
    (not= (at min) (at max))))

(defn pass-valid-2? [pass]
  (letfn [[xor [b1 b2] (or (and b1 (not b2)) (and (not b1) b2))]]
    (pass-valid? pass valid-2?)))

;;; ans-02-2: 249
(count (filter pass-valid-2? (u/read-lines "input02")))
