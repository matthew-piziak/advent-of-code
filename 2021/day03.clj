(ns aoc.day03
  (:require [aoc.utils :as u]))

(def example (u/read-lines "example03"))

(defn col-modes [in]
  (for [n (range (count (first in)))]
    (key (first (reverse (sort-by val (frequencies (map #(nth % n) in))))))))

(defn gamma [in]
  (Integer/parseInt (apply str (col-modes in)) 2))

(defn flip-bits [bs]
  (replace {\0 \1 \1 \0} bs))

(defn epsilon [in]
  (Integer/parseInt (apply str (flip-bits (col-modes in))) 2))

(defn power [in]
  (apply * ((juxt gamma epsilon) in)))

;;; ex03-1: 198
(power example)

(def input (u/read-lines "input03"))

;;; ans03-1: 2640986
(power input)

;;; from bit frequencies, choose the correct bit with tie breaking
(defn winning-bit [freqs dir]
  (if (apply = (map second freqs)) (case dir :asc \1 :desc \0)
      (first ((case dir :asc second :desc first) (sort-by val freqs)))))

;;; perform the search for oxygen and carbon dioxide
(defn diag [in n dir]
  (let [common (winning-bit (frequencies (map #(nth % n) in)) dir)
         cs (filter #(= common (nth % n)) in)]
     (if (<= (count cs) 1) (Integer/parseInt (first cs) 2)
         (recur cs (inc n) dir))))

(defn ox
  ([in] (ox in 0))
  ([in n] (diag in n :asc)))

(defn co2
  ([in] (co2 in 0))
  ([in n] (diag in n :desc)))

(defn life-support [in]
  (apply * ((juxt ox co2) in)))

;;; ex03-2: 230
(life-support example)

;;; ans03-2: 6822109
(life-support input)
