(ns aoc.day02
    (:require [aoc.utils :as u]
              [clojure.string :as s]
              [clojure.set :as set]))

(def ->rps {:X :r :Y :p :Z :s :A :r :B :p :C :s})

(defn parse-round [r]
  (into [] (map keyword (s/split r #" "))))

(defn shape-score [[_ p2]]
  (case p2 :r 1 :p 2 :s 3))

(def outcome {:p {:p :d :r :l :s :w} :r {:p :w :r :d :s :l} :s {:p :l :r :w :s :d}})

(def strat (update-vals outcome set/map-invert))

(def outcome-score {:w 6 :d 3 :l 0})

(defn round-score [r]
  (+ (shape-score r) (outcome-score (reduce get outcome r))))

(defn total-score [input]
  (->> input u/read-lines (map parse-round) (map #(map ->rps %)) (map round-score) (apply +)))

;;; ex 02-1: 15
(total-score "example02")

;;; ans 02-1: 11906
(total-score "input02")

(defn ->wld [[abc s]]
  [(->rps abc) (case s :X :l :Y :d :Z :w)])

(defn strat-score [[rps wld]]
  (case [rps wld] (round-score [rps (reduce get strat [rps wld])])))

(defn total-strat-score [input]
  (->> input u/read-lines (map parse-round) (map ->wld) (map strat-score) (apply +)))

;;; ex 02-2: 12
(total-strat-score "example02")

;;; ans 02-2: 11186
(total-strat-score "input02")
