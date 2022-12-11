(ns aoc.day09
    (:require [aoc.utils :as u]
              [clojure.string :as s]
              [clojure.set :as set]))

(defn parse-motion [l]
  (let [[d n] (s/split l #" ")]
    [(keyword d) (Integer/parseInt n)]))

(defn ->steps [[d n]]
  (repeat n d))

(defn parse-to-steps [in]
  (mapcat ->steps (map parse-motion (u/read-lines in))))

(defn step-head [[x y] d]
  (case d :R [x (inc y)] :L [x (dec y)] :U [(inc x) y] :D [(dec x) y]))

(defn dist [[hx hy] [tx ty]]
  (max (Math/abs (- hx tx)) (Math/abs (- hy ty))))

(defn adj? [h t]
  (<= (dist h t) 1))

(defn signum [n]
  (cond (pos? n) 1 (neg? n) -1 :else 0))

(defn step-tail [[hx hy] [tx ty]]
  (let [dx (- hx tx)
        dy (- hy ty)]
    [(+ tx (signum dx))
     (+ ty (signum dy))]))

(defn step-knots [knots]
  (cond
    (= (count knots) 1) knots
    (adj? (first knots) (first (rest knots))) knots
    :else
    (conj (step-knots (conj (rest (rest knots)) (step-tail (first knots) (first (rest knots))))) (first knots))))

(defn count-tail-locs [knots tlocs steps]
  (if (empty? steps) tlocs
      (let [next-head (step-head (first knots) (first steps))
            next-knots (step-knots (conj (rest knots) next-head))]
        (recur next-knots (conj tlocs (last next-knots)) (rest steps)))))

;;; ex 09-1: 13
(count (count-tail-locs (repeat 2 [0 0]) #{[0 0]} (parse-to-steps "example09")))

;;; ans 09-1: 6337
(count (count-tail-locs (repeat 2 [0 0]) #{[0 0]} (parse-to-steps "input09")))

;;; ex 09-1: 36
(count (count-tail-locs (repeat 10 [0 0]) #{[0 0]} (parse-to-steps "large09")))

;;; ans 09-2: 2455
(count (count-tail-locs (repeat 10 [0 0]) #{[0 0]} (parse-to-steps "input09")))
