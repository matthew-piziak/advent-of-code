(ns aoc.day13
    (:require [aoc.utils :as u]
              [clojure.core.matrix :as mat]))

(defn sig [rank]
  (->> rank (replace {\# 1 \. 0}) (map-indexed (fn [i x] (bit-shift-left x i))) (apply +)))

(defn ->sigs [pat]
  (mapv sig pat))

(defn refl? [sigs i j]
  (cond
    (or (< i 0) (>= j (count sigs))) true
    (not= (get sigs i) (get sigs j)) false
    :else (recur sigs (dec i) (inc j))))

(defn find-refls [sigs]
  (for [i (range (dec (count sigs))) :when (refl? sigs i (inc i))] (inc i)))

(defn para->pat [para]
  (->> para (map seq) mat/matrix))

(defn score [pat]
  (let [[vrefls hrefls] (map (comp find-refls ->sigs) ((juxt identity mat/transpose) pat))]
    (+ (apply + hrefls) (apply + (map #(* 100 %) vrefls)))))

;;; ex 13-1: 405
(transduce (map (comp score para->pat)) + 0 (u/read-paras "example13"))

;;; ans 13-1: 34772
(transduce (map (comp score para->pat)) + 0 (u/read-paras "input13"))

(defn power-of-two? [n]
  (and (> n 0) (zero? (bit-and n (dec n)))))

(defn refl-smudge?
  ([pat sigs i j] (refl-smudge? pat sigs i j false))
  ([pat sigs i j smudged?]
   (cond
     (or (< i 0) (>= j (count sigs))) smudged?
     (= (get sigs i) (get sigs j)) (recur pat sigs (dec i) (inc j) smudged?)
     smudged? false
     (power-of-two? (Math/abs (- (get sigs i) (get sigs j))))
     (if-not (= ((frequencies (map = (get pat i) (get pat j))) false) 1) false
             (recur pat sigs (dec i) (inc j) true))
     :else false)))

(defn find-refls-smudged [pat sigs]
  (for [i (range (dec (count sigs))) :when (refl-smudge? pat sigs i (inc i))] (inc i)))

(defn score-smudged [pat]
  (let [[vrefls hrefls] (map (comp (partial find-refls-smudged pat) ->sigs) ((juxt identity mat/transpose) pat))]
    (+ (apply + hrefls) (apply + (map #(* 100 %) vrefls)))))

;;; ex 13-2: 400
(transduce (map (comp score-smudged para->pat)) + 0 (u/read-paras "example13"))

;;; ans 13-2: 35554
(transduce (map (comp score-smudged para->pat)) + 0 (u/read-paras "input13"))
