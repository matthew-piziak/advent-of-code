(ns aoc.day03
  (:require [aoc.utils :as u]
            [clojure.set :as set]))

(def c->n (zipmap "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 1 53)))

(defn intersection-sum [rs]
  (transduce (map #(->> % (map (partial into #{})) (apply set/intersection) first c->n)) + rs))

(defn total-shared-priorities [input]
  (->> input u/read-lines (map #(partition (/ (count %) 2) %)) intersection-sum))

;;; ex 03-1: 157
(total-shared-priorities "example03")

;;; ans 03-1: 8139
(total-shared-priorities "input03")

(defn total-badge-priorities [input]
  (->> input u/read-lines (partition 3) intersection-sum))

;;; ex 03-2: 70
(total-badge-priorities "example03")

;;; ans 03-2: 2668
(total-badge-priorities "input03")
