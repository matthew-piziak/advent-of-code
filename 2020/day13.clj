(ns aoc.day13
  (:require [aoc.utils :as u]))

(defn parse-1 [in]
  (let [[t bs] (u/read-lines in)]
    [(Integer/parseInt t), (map #(Integer/parseInt %) (filter #(not= "x" %) (u/uncommas bs)))]))

(defn min-wait [[t bs]]
  (map (fn [b] [b (- b (mod t b))]) bs))

;;; ans 13-1: 23×5=115
(apply * (first (sort-by second (min-wait (parse-1 "input13")))))
