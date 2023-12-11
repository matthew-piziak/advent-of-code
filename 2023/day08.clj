(ns aoc.day08
    (:require [aoc.utils :as u]
              [instaparse.core :as p]
              [clojure.string :as s]
              [fastmath.core :as math]))

(defn read-map [input]
  (let [parse (p/parser "m = insts <nl> <nl> (branch <nl>)+
                       insts = inst+
                       inst = 'R' | 'L'
                       branch = node <' = ('> node <', '> node <')'>
                       node = #'[12A-Z][12A-Z][12A-Z]'
                       nl = #'\\n'+")
        transform {:inst (fn [i] (case i "L" (fn [b] (first b)) "R" (fn [b] (second b)) i))
                   :insts (fn [& is] (fn [] (cycle is)))
                   :branch (fn [[_ n] [_ b1] [_ b2]] {n [b1 b2]})
                   :m (fn [is & bs] {:insts is :branches (apply merge bs)})}]
    (->> input u/read-resource parse (p/transform transform))))

(defn steps
  ([m] (steps "AAA" ((:insts m)) (:branches m) 0))
  ([curr insts branches steps]
   (if (= curr "ZZZ") steps
       (recur ((first insts) (branches curr)) (rest insts) branches (inc steps)))))

;;; exs 08-1: 2, 6
(steps (read-map "example08"))
(steps (read-map "large08"))

;; ans 08-1: 19951
(steps (read-map "input08"))

(defn ghost-steps
  ([m]
   (let [branches (:branches m)
         starts (filter (fn [c] (s/ends-with? c "A")) (keys branches))]
     (reduce math/lcm 1 (pmap #(ghost-steps % ((:insts m)) branches 0) starts))))
  ([curr insts branches steps]
   (if (s/ends-with? curr "Z") steps
       (recur ((first insts) (branches curr)) (rest insts) branches (inc steps)))))

;;; ex 08-2: 6
(ghost-steps (read-map "example08"))

;;; ans 08-2: 16342438708751
(ghost-steps (read-map "input08"))
