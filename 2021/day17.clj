(ns aoc.day17
  (:require [aoc.utils :as u]))

(def example [20 30 -10 -5])

(def input [102 157 -146 -90])

(defn on-target? [[x y] [xmin xmax ymin ymax]]
  (and (<= xmin x xmax) (<= ymin y ymax)))

(defn out? [[x y] [xmin xmax ymin ymax]]
  (or (< y ymin) (> x xmax)))

(def initial-pos [0 0])

(defn step [target [x y] [dx dy] max-steps max-height]
  (cond
    (on-target? [x y] target) max-height
    (zero? max-steps) nil
    :else (recur target [(+ x dx) (+ y dy)] [(- dx (Integer/signum dx)) (dec dy)] (dec max-steps) (max max-height y))))

;; ans17-1: 10585
(defn find-max [[rdx rdy] steps max-so-far]
  (println [rdx rdy steps])
  (let [ms (remove nil? (for [dx (range (* -1 rdx) rdx)
                              dy (range (* -1 rdy) rdy)]
                          (step input initial-pos [dx dy] steps 0)))
        m (if (empty? ms) 0 (apply max ms))]
    (if (> m max-so-far)
      (do
        (println m)
        (recur [(* rdx 2) (* rdy 2)] (* steps 2) m))
      (recur [(* rdx 2) (* rdy 2)] (* steps 2) max-so-far))))

;;; dirty search is sufficient for this one
(defn step-all [target [x y] [dx dy]]
  (cond
    (out? [x y] target) 0
    (on-target? [x y] target) 1
    :else (recur target [(+ x dx) (+ y dy)] [(- dx (Integer/signum dx)) (dec dy)])))

;;; ex17-2
(apply + (for [dx (range 1 100) dy (range -100 100)] (step-all example initial-pos [dx dy])))

(defn find-all [input m]
  (println (str "Step: " m))
  (let [s (apply + (for [dx (range 1 m) dy (range (- m) m)] (step-all input initial-pos [dx dy])))]
    (println "Vals: " s)
    (recur input (* m 2))))

;;; ans17-2: 5247
(find-all input 1)
