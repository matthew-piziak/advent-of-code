(ns aoc.day06
  (:require [aoc.utils :as u]))

(def example ["mjqjpqmgbljsphdztnvjfqwrcgsmlb" "bvwbjplbgvbhsrlpgdmjqwftvncz" "nppdvjthqldpwncqszvftbrmjlhg"
              "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn detect [n in]
  (->> (partition n 1 in) (keep-indexed #(when (apply distinct? %2) (+ %1 n))) first))

;;; ex 06-1: [7 5 6 10 11]
(mapv (partial detect 4) example)

(def input (u/read-resource "input06"))

;;; ans 06-1: 1042
(detect 4 input)

;;; ex 06-2: [19 23 23 29 26]
(mapv (partial detect 14) example)

;;; ans 06-2: 2980
(detect 14 input)
