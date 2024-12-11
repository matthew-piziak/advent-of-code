(ns aoc.day11
  (:require [aoc.utils :as u]))

(defn read-stones [in]
  (->> in u/read-resource (re-seq #"\d+") (map #(Integer/parseInt %)) frequencies))

(defn split-stone [stone mag]
  (let [s (str stone) k (count s) h (/ k 2)]
    (merge-with + {(Integer/parseInt (apply str (take h s))) mag} {(Integer/parseInt (apply str (drop h s))) mag})))

(defn blink [stone mag]
  (cond
    (zero? stone) {1 mag}
    (even? (count (str stone))) (split-stone stone mag)
    :else {(* 2024 stone) mag}))

(defn blinks [stones times]
  (if (zero? times) (apply + (vals stones))
      (recur (apply (partial merge-with +) (map (fn [[stone mag]] (blink stone mag)) stones)) (dec times))))

(blinks (read-stones "example11") 1)    ; ex 11-1: 7
(blinks (read-stones "large11") 25)     ; lg 11-1: 55312
(blinks (read-stones "input11") 25)     ; an 11-1: 233050

(blinks (read-stones "input11") 75)     ; an 11-2: 276661131175807
