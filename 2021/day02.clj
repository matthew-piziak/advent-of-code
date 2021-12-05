(ns aoc.day02
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(def example (u/read-lines "example02"))

(defn parse-line [line]
  (let [[_ dir n] (re-find #"(forward|down|up) ([0-9]+)" line)]
    {:dir dir :n (Long/parseLong n)}))

(defn steps [input]
  (map parse-line input))

(defn nav
  ([steps] (nav 0 0 steps))
  ([depth forward steps]
   (if (empty? steps) (* depth forward)
       (let [step (first steps)]
         (case (step :dir)
           "forward" (recur depth (+ forward (step :n)) (rest steps))
           "down" (recur (+ depth (step :n)) forward (rest steps))
           "up" (recur (- depth (step :n)) forward (rest steps)))))))

;;; ex02-1: 150
(nav (steps example))

(def input (u/read-lines "input02"))

;;; ans02-1: 1507611
(nav (steps input))

(defn nav2
  ([steps] (nav2 0 0 0 steps))
  ([depth forward aim steps]
   (if (empty? steps) (* depth forward)
       (let [step (first steps)]
         (case (step :dir)
           "forward" (recur (+ depth (* aim (step :n))) (+ forward (step :n)) aim (rest steps))
           "down" (recur depth forward (+ aim (step :n)) (rest steps))
           "up" (recur depth forward (- aim (step :n)) (rest steps)))))))

;;; ex02-2: 900
(nav2 (steps example))

;;; ans02-2: 1880593125
(nav2 (steps input))
