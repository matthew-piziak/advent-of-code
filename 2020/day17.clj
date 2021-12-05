(ns aoc.day17
    (:require [aoc.utils :as u]
              [clojure.pprint :as p]))

(defn moore-neighborhood [[x y z]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not (= [dx dy dz] [0 0 0]))]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn step [set-of-cells]
  (set (for [[cell count] (frequencies (mapcat moore-neighborhood set-of-cells))
             :when (or (= 3 count)
                       (and (= 2 count) (contains? set-of-cells cell)))]
         cell)))

(defn print-world [cells]
  (p/pprint (count cells)))

(defn run-life [num-steps set-of-cells]
  (loop [s num-steps
         cells set-of-cells]
    (print-world cells)
    (when (< 0 s)
      (recur (- s 1) (step cells)))))

(def *glider* #{[1 0 0] [2 1 0] [0 2 0] [1 2 0] [2 2 0]})

(def *input* #{        [0 1 0]         [0 3 0]                 [0 6 0] [0 7 0]
                               [1 2 0]                                 [1 7 0]
               [2 0 0] [2 1 0]         [2 3 0] [2 4 0] [2 5 0] [2 6 0]
                                       [3 3 0] [3 4 0] [3 5 0] [3 6 0]
               [4 0 0]         [4 2 0] [4 3 0]                 [4 6 0] [4 7 0]
               [5 0 0]                         [5 4 0] [5 5 0]
                                       [6 3 0] [6 4 0]         [6 6 0] [6 7 0]
               [7 0 0]                         [7 4 0]         [7 6 0]})

;;; ans 17-1: 317
(run-life 6 *input*)

(defn moore-hyper-neighborhood [[x y z h]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        dh [-1 0 1]
        :when (not (= [dx dy dz dh] [0 0 0 0]))]
    [(+ x dx) (+ y dy) (+ z dz) (+ h dh)]))

(defn hyper-step [set-of-cells]
  (set (for [[cell count] (frequencies (mapcat moore-hyper-neighborhood set-of-cells))
             :when (or (= 3 count)
                       (and (= 2 count) (contains? set-of-cells cell)))]
         cell)))

(defn run-hyper-life [num-steps set-of-cells]
  (loop [s num-steps
         cells set-of-cells]
    (print-world cells)
    (when (< 0 s)
      (recur (- s 1) (hyper-step cells)))))

(def *hyper-glider* #{[1 0 0 0] [2 1 0 0] [0 2 0 0] [1 2 0 0] [2 2 0 0]})

(def *hyper-input* #{
             [0 1 0 0] [0 3 0 0] [0 6 0 0] [0 7 0 0]
             [1 2 0 0] [1 7 0 0]
             [2 0 0 0] [2 1 0 0] [2 3 0 0] [2 4 0 0] [2 5 0 0] [2 6 0 0]
             [3 3 0 0] [3 4 0 0] [3 5 0 0] [3 6 0 0]
             [4 0 0 0] [4 2 0 0] [4 3 0 0] [4 6 0 0] [4 7 0 0]
             [5 0 0 0] [5 4 0 0] [5 5 0 0]
             [6 3 0 0] [6 4 0 0] [6 6 0 0] [6 7 0 0]
             [7 0 0 0] [7 4 0 0] [7 6 0 0]})

;;; ex 17-2: 848
(run-hyper-life 6 *hyper-glider*)

(run-hyper-life 6 *hyper-input*)
