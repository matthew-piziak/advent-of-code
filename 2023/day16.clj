(ns aoc.day16
  (:require [aoc.utils :as u]
            [aoc.grid :as g]))

(def example (g/read-grid "example16"))

(def start [:rt [0 0]])

(defn next [grid [d [x y]]]
  (case (grid [x y])
    nil []
    \. [(case d :up [:up [x (dec y)]] :dn [:dn [x (inc y)]] :rt [:rt [(inc x) y]] :lt [:lt [(dec x) y]])]
    \/ [(case d :up [:rt [(inc x) y]] :dn [:lt [(dec x) y]] :rt [:up [x (dec y)]] :lt [:dn [x (inc y)]])]
    \\ [(case d :up [:lt [(dec x) y]] :dn [:rt [(inc x) y]] :rt [:dn [x (inc y)]] :lt [:up [x (dec y)]])]
    \| (case d :up [[:up [x (dec y)]]] :dn [[:dn [x (inc y)]]] (:rt :lt) [[:up [x (dec y)]] [:dn [x (inc y)]]])
    \- (case d (:up :dn) [[:rt [(inc x) y]] [:lt [(dec x) y]]] :rt [[:rt [(inc x) y]]] :lt [[:lt [(dec x) y]]])))

(defn beam [grid seen frontier]
  (if (empty? frontier) seen
      (recur grid (apply conj seen frontier) (remove seen (mapcat (partial next grid) frontier)))))

(defn energized [grid seen]
  (count (into #{} (filter grid (map second seen)))))

;;; ex 16-1: 46
(energized example (beam example #{} (list start)))

(def input (g/read-grid "input16"))

;;; ans 16-1: 8901
(energized input (beam input #{} (list start)))

(defn starts [size]
  (mapcat
   (juxt (fn [n] [:rt [0 n]]) (fn [n] [:lt [(dec size) n]]) (fn [n] [:up [n (dec size)]]) (fn [n] [:dn [n 0]]))
   (range 0 size)))

;;; ex 16-1: 51
(apply max (map #(energized example (beam example #{} %)) (map list (starts 10))))

;;; ans 16-1: 9064
(apply max (map #(energized input (beam input #{} %)) (map list (starts 110))))
