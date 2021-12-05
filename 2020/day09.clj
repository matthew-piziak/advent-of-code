(ns aoc.day09
  (:require [aoc.utils :as u]
            [clojure.math.combinatorics :as combo]))

(def example (map #(Long/parseLong %) (u/read-lines "example09")))

(def input (map #(Long/parseLong %) (u/read-lines "input09")))

(defn invalid? [p]
  (let [v (last p)
        pre (butlast p)]
    (not (some #{v} (map (partial apply +) (combo/combinations pre 2))))))

(defn first-invalid [in pre-count]
  (let [ps (partition (inc pre-count) 1 in)]
    (last (first (filter invalid? ps)))))

(def exa-09-1 (first-invalid example 5))

;; ans 09-1: 1930745883
(def ans-09-1 (first-invalid input 25))

(defn weakness [in inv]
  (let [s (first (filter #(= inv (apply + %)) (apply concat (map #(partition % 1 in) (range 2 (count in))))))]
    (+ (apply min s) (apply max s))))

(weakness example exa-09-1)

;; ans 09-2: 268878261
(weakness input ans-09-1)
