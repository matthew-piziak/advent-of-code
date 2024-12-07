(ns aoc.day07
  (:require [aoc.utils :as u]))

(defn cal?
  ([ops [t & [n & ns]]] (cal? ops t ns n))
  ([ops test inputs acc]
   (if (empty? inputs) (or (= acc test) nil)
     (let [[n & ns] inputs]
       (some identity (map #(cal? ops test ns (% acc n)) ops))))))

(defn cal-res [f in]
  (u/sum-lines in (comp (map #(re-seq #"\d+" %)) (map (partial map #(Long/parseLong %))) (filter f) (map first))))

(cal-res (partial cal? [* +]) "example07")              ; ex 07-1: 3749
(cal-res (partial cal? [* +]) "input07")                ; an 07-1: 7885693428401

(def ct #(Long/parseLong (str (str %1) (str %2))))

(cal-res (partial cal? [* + ct]) "example07")           ; ex 07-2: 11387
(cal-res (partial cal? [* + ct]) "input07")             ; an 07-2: 348360680516005
