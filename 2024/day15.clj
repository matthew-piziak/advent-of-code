(ns aoc.day15
  (:require [aoc.grid :as g]
            [aoc.utils :as u]
            [clojure.set :as set]))

(defn next-loc [[x y] move]
  (case move \< [(dec x) y] \> [(inc x) y] \^ [x (dec y)] \v [x (inc y)]))

(defn step [grid loc move & no-mover-check]
  (let [mover (grid loc)]
    (cond
      (contains? #{\# \.} mover) grid
      (and (contains? #{\[ \]} mover) (contains? #{\^ \v} move) (not no-mover-check))
      (let [side-loc (next-loc loc (case mover \[ \> \] \<))
            side-grid (step grid side-loc move :no-mover-check)
            next-loc (next-loc loc move)
            new-grid (step side-grid next-loc move)]
        (if-not (and (= (new-grid next-loc) \.) (= (side-grid side-loc) \.)) grid
                (-> new-grid (assoc next-loc mover) (assoc loc \.))))
      :else
      (let [next-loc (next-loc loc move)
            new-grid (step grid next-loc move)]
        (if-not (= (new-grid next-loc) \.) grid
                (-> new-grid (assoc next-loc mover) (assoc loc \.)))))))

(defn robot-loc [grid loc next-loc]
  (cond (= \@ (grid loc)) loc (= \@ (grid next-loc)) next-loc :else :robot-not-found))

(defn steps [grid moves loc]
  (if (empty? moves) grid
      (let [new-grid (step grid loc (first moves))
            next-loc (next-loc loc (first moves))]
        (recur new-grid (rest moves) (robot-loc new-grid loc next-loc)))))

(defn score [[x y]] (+ (* 100 y) x))
(def box-score (comp (filter (fn [[_ v]] (= v \O))) (map (fn [[loc _]] (score loc)))))
(def wide-score (comp (filter (fn [[_ v]] (= v \[))) (map (fn [[loc _]] (score loc)))))

(defn widen-tile [[x y] tile]
  (case tile
    \# [[[(* 2 x) y] \#] [[(inc (* 2 x)) y] \#]]
    \O [[[(* 2 x) y] \[] [[(inc (* 2 x)) y] \]]]
    \. [[[(* 2 x) y] \.] [[(inc (* 2 x)) y] \.]]
    \@ [[[(* 2 x) y] \@] [[(inc (* 2 x)) y] \.]]))

(defn widen-grid [grid]
  (into {} (apply concat (map (partial apply widen-tile) grid))))

(defn gps [xform tag & widen]
  (let [grid ((if widen widen-grid identity) (g/read-grid (str tag "map" "15")))
        moves (apply concat (u/read-lines (str tag "moves" "15")))
        start ((set/map-invert grid) \@)]
    (transduce xform + 0 (steps grid moves start))))

(gps box-score "small")           ; sm 15-1: 2028
(gps box-score "example")         ; ex 15-1: 10092
(gps box-score "input")           ; in 15-1: 1429911
(gps wide-score "example" :widen) ; ex 15-2: 9021
(gps wide-score "input" :widen)   ; an 15-2: 1453087
