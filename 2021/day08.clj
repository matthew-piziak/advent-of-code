(ns aoc.day08
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as set]))

(def single "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(def example (u/read-lines "example08"))

(defn part-one [in]
  (let [f (frequencies (map count (apply concat (map (fn [l] (s/split (second (s/split l #" \| ")) #" ")) in))))]
    (apply + [(f 2) (f 4) (f 3) (f 7)])))

;;; ex08-1: 26
(part-one example)

(def input (u/read-lines "input08"))

;;; ans08-1: 532
(part-one input)

(defn segments [line]
  (group-by count (map set (s/split (first (s/split line #" \| ")) #" "))))

(defn code [line]
  (map set (s/split (second (s/split line #" \| ")) #" ")))

(defn segmap [line]
  ;; One by one, we use process of elimination to identify each number
  (let [segments (segments line)
        [one] (segments 2)
        [four] (segments 4)
        [seven] (segments 3)
        [eight] (segments 7)
        [two] (filter #(= 3 (count (set/difference % four))) (segments 5))
        [three] (filter #(= 1 (count (set/difference % two))) (segments 5))
        [five] (filter #(= 2 (count (set/difference % two))) (segments 5))
        [six] (filter #(= 4 (count (set/difference % seven))) (segments 6))
        [nine] (filter #(and (not= six %) (= 1 (count (set/difference % five)))) (segments 6))
        [zero] (filter #(= 2 (count (set/difference % five))) (segments 6))]
    {zero \0 one \1 two \2 three \3 four \4 five \5 six \6 seven \7 eight \8 nine \9}))

(defn decode [line]
  (let [segmap (segmap line)
        code (code line)]
    (Long/parseLong (apply str (map segmap code)))))

(defn part-two [in]
  (apply + (map decode in)))

;;; ex08-2: 61229
(part-two example)

;;; ans08-2: 1011284
(part-two input)
