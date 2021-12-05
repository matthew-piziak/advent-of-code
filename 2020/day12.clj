(ns aoc.day12
  (:require [aoc.utils :as u]))

(defn parse [l]
  (let [[_ ins num] (re-find #"([NSEWLRF])(\d+)" l)]
    [ins (Integer/parseInt num)]))

(def example (map parse (u/read-lines "example12")))
(def input (map parse (u/read-lines "input12")))

(def origin-1 [[0 0] 0])

(defn step-1 [[[x y] deg] [ins num]]
  (case ins
    "N" [[x (+ y num)] deg]
    "S" [[x (- y num)] deg]
    "E" [[(+ x num) y] deg]
    "W" [[(- x num) y] deg]
    "L" [[x y] (+ deg num)]
    "R" [[x y] (- deg num)]
    "F" [[(+ x (* (Math/cos (Math/toRadians deg)) num)) (+ y (* (Math/sin (Math/toRadians deg)) num))] deg]))

(defn m-dist-1 [[[x y] _]]
  (+ (Math/abs x) (Math/abs y)))

;;; ans 12-1: 2297
(m-dist-1 (reduce step-1 origin-1 input))

(def origin-2 [[10 1] [0 0]])

(defn step-2 [[[wx wy] [sx sy]] [ins num]]
  (case ins
    "N" [[wx (+ wy num)] [sx sy]]
    "S" [[wx (- wy num)] [sx sy]]
    "E" [[(+ wx num) wy] [sx sy]]
    "W" [[(- wx num) wy] [sx sy]]
    "L" [(case num 90 [(- wy) wx] 180 [(- wx) (- wy)] 270 [wy (- wx)]) [sx sy]]
    "R" (step-2 [[wx wy] [sx sy]] ["L" (- 360 num)])
    "F" [[wx wy] [(+ sx (* num wx)) (+ sy (* num wy))]]))

(defn m-dist-2 [[_ [x y] _]]
  (+ (Math/abs x) (Math/abs y)))

;;; ans 12-2: 89984
(m-dist-2 (reduce step-2 origin-2 input))
