(ns aoc.day01
  (:require [aoc.utils :as u]))

(defn pack->calories [pack]
  (map (comp (partial apply +) (partial map #(Integer/parseInt %))) pack))

(defn most-caloric-elf [input]
  (->> input u/read-paras pack->calories (apply max)))

;;; ex 01-1: 24000
(most-caloric-elf "example01")

;;; ans 01-2: 72070
(most-caloric-elf "input01")

(defn top-three-elves [input]
  (->> input u/read-paras pack->calories sort (take-last 3) (apply +)))

;;; ex 01-2: 45000
(top-three-elves "example01")

;;; ans 01-2: 211805
(top-three-elves "input01")
