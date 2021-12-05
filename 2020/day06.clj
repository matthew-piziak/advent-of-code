(ns aoc.day06
    (:require [aoc.utils :as u]
              [clojure.set :as set]))

(def input (u/read-paras "input06"))

;;; ans 06-1: 6885
(apply + (map count (map frequencies (map (partial apply str) input))))

;;; ans 06-2: 3550
(apply + (map count (map #(apply set/intersection %) (map #(map set %) input))))
