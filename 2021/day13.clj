(ns aoc.day13
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn parse-dots [in]
  (->> in
     first
     (map #(map (fn [n] (Long/parseLong n)) (s/split % #",")))
     (into #{})))

(defn fold-at [n fold]
  (if (< n fold) n (- fold (- n fold))))

(defn fold-y [dots fold]
  (->> dots
     (remove (fn [[x y]] (= y fold)))
     (into #{} (map (fn [[x y]] [x (fold-at y fold)])))))

(defn fold-x [dots fold]
  (->> dots
     (remove (comp (partial = fold) first))
     (into #{} (map (fn [[x y]] [(fold-at x fold) y])))))

(def example (u/read-paras "example13"))

;;; ex 13-1: 17
(count (-> example parse-dots (fold-y 7)))

(def input (u/read-paras "input13"))

;;; ans13-1: 724
(count (-> input parse-dots (fold-x 655)))

(defn print-dot-line [dots]
  (doseq [n (range 0 (inc (apply max dots)))]
    (print (if (dots n) "X" " ")))
  (println))

;;; ans13-2: CPJBERUL
(doseq [l (->> (-> input parse-dots
              (fold-x 655)
              (fold-y 447)
              (fold-x 327)
              (fold-y 223)
              (fold-x 163)
              (fold-y 111)
              (fold-x 81)
              (fold-y 55)
              (fold-x 40)
              (fold-y 27)
              (fold-y 13)
              (fold-y 6))
           (group-by first)
           (sort))]
  (print-dot-line (into #{} (map second (second l)))))

;;  XXXX
;; X    X    Just another Perl hacker...
;; X    X
;;  X  X
;; XXXXXX
;; X  X
;; X  X
;;  XX
;;     X
;;      X
;; X    X
;; XXXXX
;; XXXXXX
;; X X  X
;; X X  X
;;  X XX
;; XXXXXX
;; X X  X
;; X X  X
;; X    X
;; XXXXXX
;; X  X
;; X  XX
;;  XX  X
;; XXXXX
;;      X
;;      X
;; XXXXX
;; XXXXXX
;;      X
;;      X
;;      X
