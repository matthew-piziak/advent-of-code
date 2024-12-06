(ns aoc.day06
  (:require
   [aoc.utils :as u]
   [aoc.grid :as g]
   [clojure.set :as set]))

(defn start [grid] ((set/map-invert grid) \^))
(defn turn-right [orientation] (case orientation :up :rt :rt :dn :dn :lt :lt :up))
(defn front [[x y] orientation] (case orientation :up [x (dec y)] :rt [(inc x) y] :dn [x (inc y)] :lt [(dec x) y]))
(defn blocked? [grid [x y] orientation] (= (grid (front [x y] orientation)) \#))

(defn walk
  ([grid] (let [s (start grid)] (walk grid s #{} :up)))
  ([grid curr seen orientation]
   (cond (nil? (grid curr)) (->> seen (map first) set count)
         (contains? seen [curr orientation]) :loop
         (blocked? grid curr orientation) (recur grid curr seen (turn-right orientation))
         :else (recur grid (front curr orientation) (conj seen [curr orientation]) orientation))))

(walk (g/read-grid "example06"))        ; ex 06-1: 41
(walk (g/read-grid "input06"))          ; an 06-1: 4977

(defn loop-obstructions [in]
  (let [grid (g/read-grid in)
        coords (->> in u/read-lines g/coords)]
    (->> coords (map (comp walk #(assoc grid % \#))) frequencies :loop)))

(loop-obstructions "example06")         ; ex 06-2: 6
(loop-obstructions "input06")           ; an 06-2: 1729
