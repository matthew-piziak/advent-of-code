(ns aoc.day06
  (:require [aoc.utils :as u]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

(def example (frequencies [3 4 3 1 2]))

(defn age [freqs]
  (set/rename-keys freqs (apply merge (map (fn [n m] {n m}) (keys freqs) (map dec (keys freqs))))))

(defn repr [freqs n gens]
  (if (zero? gens) n
      (let [zeroes (or (freqs 0) 0)
            next (-> freqs (dissoc 0) age (assoc 8 zeroes) (update 6 (fn [s] (+ (or s 0) zeroes))))]
        (recur next (+ n zeroes) (dec gens)))))


;;; ex06-1: 5934
(repr example 5 80)

(def input
  (frequencies [1 1 3 1 3 2 1 3 1 1 3 1 1 2 1 3 1 1 3 5 1 1 1 3 1 2 1 1 1 1 4 4 1 2 1 2 1 1 1 5 3 2 1 5 2 5 3 3 2 2 5 4 1 1 4 4 1 1 1 1 1 1 5 1 2 4 3 2 2 2 2 1 4 1 1 5 1 3 4 4 1 1 3 3 5 5 3 1 3 3 3 1 4 2 2 1 3 4 1 4 3 3 2 3 1 1 1 5 3 1 4 2 2 3 1 3 1 2 3 3 1 4 2 2 4 1 3 1 1 1 1 1 2 1 3 3 1 2 1 1 3 4 1 1 1 1 5 1 1 5 1 1 1 4 1 5 3 1 1 3 2 1 1 3 1 1 1 5 4 3 3 5 1 3 4 3 3 1 4 4 1 2 1 1 2 1 1 1 2 1 1 1 1 1 5 1 1 2 1 5 2 1 1 2 3 2 3 1 3 1 1 1 5 1 1 2 1 1 1 1 3 4 5 3 1 4 1 1 4 1 4 1 1 1 4 5 1 1 1 4 1 3 2 2 1 1 2 3 1 4 3 5 1 5 1 1 4 5 5 1 1 3 3 1 1 1 1 5 5 3 3 2 4 1 1 1 1 1 5 1 1 2 5 5 4 2 4 4 1 1 3 3 1 5 1 1 1 1 1 1]))

(def input-n 300)

;;; ans06-1: 373378
(repr input input-n 80)

;;; ex06-2: 26984457539
(repr example 5 256)

;;; ans06-2: 1682576647495
(repr input input-n 256)
