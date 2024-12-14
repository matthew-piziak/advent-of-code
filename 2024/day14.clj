(ns aoc.day14
  (:require [instaparse.core :as p]
            [aoc.utils :as u]))

(defn read-robot [line]
  (let [parse (p/parser "robot = <'p='> coords <ws>  <'v='> coords
                         coords = num <','> num
                         num = #'[\\-0-9]+'
                         ws = #'\\s*' ")
        transform {:num #(Integer/parseInt %)
                   :coords (fn [x y] [x y])
                   :robot (fn [p v] {:p p :v v})}]
    (->> line parse (p/transform transform))))

(defn step [[sx sy] robot]
  (let [{[vx vy] :v} robot]
    (update robot :p (fn [[x y]] [(mod (+ vx x) sx) (mod (+ vy y) sy)]))))

(defn steps [[sx sy] n robots]
  (letfn [(simulate [r] (first (drop n (iterate (partial step [sx sy]) r))))]
    (map simulate robots)))

(defn quadrant [[sx sy] robot]
  (let [{[px py] :p} robot hx (int (/ sx 2)) hy (int (/ sy 2))]
    (cond
      (and (< px hx) (< py hy)) :ul
      (and (< px hx) (> py hy)) :ll
      (and (> px hx) (< py hy)) :ur
      (and (> px hx) (> py hy)) :lr
      :else :na)))

(defn safety
  ([[sx sy] robots]
   (apply * (-> (map (partial quadrant [sx sy]) robots) frequencies (dissoc :na) vals))))

(defn in->safety [in [sx sy] n]
  (safety [sx sy] (steps [sx sy] n (map read-robot (u/read-lines in)))))

(in->safety "example14" [7 11] 100)     ; ex 14-1: 12
(in->safety "input14" [101 103] 100)    ; an 14-1: 226179492

(defn tree
  ([in [sx sy]]
   (let [rs (map read-robot (u/read-lines in))]
     (tree rs [sx sy] (safety [sx sy] rs) 0)))
  ([robots [sx sy] lowest-safety steps-taken]
   (let [new-robots (map (partial step [sx sy]) robots)
         new-safety (safety [sx sy] robots)
         max-collisions (apply max (vals (frequencies (map :p robots))))]
     (if (and (< new-safety lowest-safety) (= 1 max-collisions)) steps-taken
         (recur new-robots [sx sy] (min new-safety lowest-safety) (inc steps-taken))))))

(tree "input14" [101 103])              ; ex 14-2: 7502
