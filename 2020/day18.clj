(ns aoc.day18
    (:require [instaparse.core :as p]
              [aoc.util :as u]))

(def ex-1 "1 + 2 * 3 + 4 * 5 + 6")
(def ex-2 "1 + (2 * 3) + (4 * (5 + 6))")
(def ex-3 "2 * 3 + (4 * 5)")
(def ex-4 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
(def ex-5 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
(def ex-6 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

(def parse
  (p/parser
   "expr = sum-prod
    <sum-prod> = term | sum | prod
    sum = sum-prod <space> <'+'> <space> term | sum-prod
    prod = sum-prod <space> <'*'> <space> term | sum-prod
    term = <'('> sum-prod <')'> | num
    num = #'[0-9]'
    space = #'\\s'"))

(defn evl [expr]
  (case (first expr)
    :expr (evl (nth expr 1))
    :sum (apply + (map evl (rest expr)))
    :prod (apply * (map evl (rest expr)))
    :term (evl (nth expr 1))
    :num (Integer/parseInt (nth expr 1))))

;;; ans 18-1: 31142189909908
(apply + (map (comp evl parse) (u/read-lines "input18")))

(def parse-add-first
  (p/parser
   "expr = sum-prod
    <sum-prod> = term | prod | sum
    prod = sum-prod <space> <'*'> <space> sum-prod
    sum = sum <space> <'+'> <space> sum | term
    term = <'('> sum-prod <')'> | num
    num = #'[0-9]'
    space = #'\\s'"))

;; ans 18-2: 323912478287549
(apply + (map (comp evl parse-add-first) (u/read-lines "input18")))
