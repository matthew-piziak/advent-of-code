(ns aoc.day02
  (:require [aoc.utils :as u]
            [instaparse.core :as p]))

(defn read-report [line]
  (let [parse (p/parser "levels = num (<ws> num)* num = #'[0-9]+' ws = #'\\s*'")
        transform {:num #(Integer/parseInt %) :levels (fn [& nums] nums)}]
    (p/transform transform (parse line))))

(defn safe? [ls] (and (or (apply > ls) (apply < ls)) (every? #(<= -3 % 3) (map - ls (rest ls)))))
(defn safe-with-mulligan? [ls] (some safe? (map #(concat (take % ls) (drop (inc %) ls)) (range (count ls)))))

(u/sum-lines "example02" (map (comp u/bool->int safe? read-report)))               ; ex 02-1: 2
(u/sum-lines "input02" (map (comp u/bool->int safe? read-report)))                 ; an 02-1: 341
(u/sum-lines "example02" (map (comp u/bool->int safe-with-mulligan? read-report))) ; ex 02-2: 4
(u/sum-lines "input02" (map (comp u/bool->int safe-with-mulligan? read-report)))   ; ans 02-2: 404
