(ns aoc.day19
    (:require [aoc.utils :as u]
              [instaparse.core :as p]
              [clojure.string :as s]))

(def parse-rule
  (p/parser
   "rule = lhs <':'> <space> rhss
    lhs = num
    rhss = rhs <space> <'|'> <space> rhs | rhs
    rhs = num | num <space> num | num <space> num <space> num | a | b
    num = #'[0-9]+'
    space = #'\\s'
    a = <'\"a\"'>
    b = <'\"b\"'>"))

;;; TODO: use :output-format :enlive to clean up nths
;;; TODO: try insta/transform
(defn metaparse-rule [[_ [_ [_ lhs]] rhss]]
  (cond
    (= (count (nth (nth rhss 1) 1)) 1) ; leaf
    (case (first (nth (nth rhss 1) 1))
       :a (format "%s = 'a'" lhs)
       :b (format "%s = 'b'" lhs))
    (and (= (count rhss) 2) (= (count (rest (nth rhss 1))) 2)) ; one sib
    (let [rhs1 (nth (nth (nth rhss 1) 1) 1)
          rhs2 (nth (nth (nth rhss 1) 2) 1)]
      (format "%s = %s %s" lhs rhs1 rhs2))
    (and (= (count rhss) 2) (= (count (rest (nth rhss 1))) 3)) ; one sib, triple
    (let [rhs1 (nth (nth (nth rhss 1) 1) 1)
          rhs2 (nth (nth (nth rhss 1) 2) 1)
          rhs3 (nth (nth (nth rhss 1) 3) 1)]
      (format "%s = %s %s %s" lhs rhs1 rhs2 rhs3))
    (and (= (count rhss) 2) (= (count (rest (nth rhss 1))) 1)) ; one sib, single
    (let [rhs1 (nth (nth (nth rhss 1) 1) 1)]
      (format "%s = %s" lhs rhs1))
    (and (= (count rhss) 3) (= (count (nth rhss 1)) 2) (= (count (nth rhss 2)) 3)) ; two sibs, single/double
    (let [rhs1 (nth (nth (nth rhss 1) 1) 1)
          rhs2 (nth (nth (nth rhss 2) 1) 1)
          rhs3 (nth (nth (nth rhss 2) 2) 1)]
      (format "%s = %s | %s %s" lhs rhs1 rhs2 rhs3))
    (and (= (count rhss) 3) (= (count (nth rhss 1)) 3) (= (count (nth rhss 2)) 4)) ; two sibs, double/triple
    (let [rhs1 (nth (nth (nth rhss 1) 1) 1)
          rhs2 (nth (nth (nth rhss 1) 2) 1)
          rhs3 (nth (nth (nth rhss 2) 1) 1)
          rhs4 (nth (nth (nth rhss 2) 2) 1)
          rhs5 (nth (nth (nth rhss 2) 3) 1)]
      (format "%s = %s %s | %s %s %s" lhs rhs1 rhs2 rhs3 rhs4 rhs5))
    (and (= (count rhss) 3) (= (count (nth rhss 1)) 3)) ; two sibs
    (let [rhs1 (nth (nth (nth rhss 1) 1) 1)
          rhs2 (nth (nth (nth rhss 1) 2) 1)
          rhs3 (nth (nth (nth rhss 2) 1) 1)
          rhs4 (nth (nth (nth rhss 2) 2) 1)]
      (format "%s = %s %s | %s %s" lhs rhs1 rhs2 rhs3 rhs4))
    (and (= (count rhss) 3) (= (count (nth rhss 1)) 2)) ; two sibs, single
    (let [rhs1 (nth (nth (nth rhss 1) 1) 1)
          rhs2 (nth (nth (nth rhss 2) 1) 1)]
      (format "%s = %s | %s" lhs rhs1 rhs2))))

(defn metaparse [in]
  (p/parser (s/join "\n" (map (comp metaparse-rule parse-rule) (u/read-lines in)))))

(defn parses? [metaparser candidate]
  (not (p/failure? (metaparser candidate))))

;;; ans 19-1: 220
(time (count (filter (partial parses? (metaparse "rules19")) (u/read-lines "candidates19"))))

;; ans 19-2: 439
(time (count (filter (partial parses? (metaparse "rules19-2")) (u/read-lines "candidates19"))))
