(ns aoc.day03
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn mul [[_ x y]] (* (Integer/parseInt x) (Integer/parseInt y)))

(defn muls [in]
  (->> in u/read-resource (re-seq #"mul\((\d+),(\d+)\)") (map mul) (apply +)))

(muls "example03") ; ex 03-1: 161
(muls "input03")   ; an 03-1: 168539636

(defn flagged-muls
  ([in] (flagged-muls (->> in u/read-resource (re-seq #"do\(\)|don\'t\(\)|mul\((\d+),(\d+)\)")) 0 true))
  ([insts acc enabled]
   (cond
     (empty? insts) acc
     (s/starts-with? (ffirst insts) "don't") (recur (rest insts) acc false)
     (s/starts-with? (ffirst insts) "do") (recur (rest insts) acc true)
     (s/starts-with? (ffirst insts) "mul") (recur (rest insts) (if enabled (+ acc (mul (first insts))) acc) enabled))))

(flagged-muls "extra03") ; ex 03-2:48
(flagged-muls "input03") ; an 03-2: 97529391
