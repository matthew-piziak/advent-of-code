(ns aoc.day14
    (:require [aoc.utils :as u]
              [clojure.set :as set]))

(defn parse-path [l]
  (->> l (re-seq #"\d+") (map #(Integer/parseInt %)) (partition 2) (map (fn [[x y]] {:x x :y y}))))

(defn rock-locs [{x :x y :y} {x' :x y' :y}]
  (set
   (cond
     (= x x') (map (fn [y''] {:x x :y y''}) (range (min y y') (inc (max y y'))))
     (= y y') (map (fn [x''] {:x x'' :y y}) (range (min x x') (inc (max x x'))))
     :invalid (throw "scan not orthogonal"))))

(defn path->rock-locs [path]
  (apply set/union (map (partial apply rock-locs) (partition 2 1 path))))

(def example (apply set/union (map path->rock-locs (->> "example14" u/read-lines (map parse-path)))))
(def input (apply set/union (map path->rock-locs (->> "input14" u/read-lines (map parse-path)))))

(def sand-source {:x 500 :y 0})
(defn dwn [{x :x y :y}] {:x x :y (inc y)})
(defn dnl [{x :x y :y}] {:x (dec x) :y (inc y)})
(defn dnr [{x :x y :y}] {:x (inc x) :y (inc y)})

(defn fall-abyss [sand rock grains]
  (cond
    (> (:y sand) 153) grains
    (not (rock (dwn sand))) (recur (dwn sand) rock grains)
    (not (rock (dnl sand))) (recur (dnl sand) rock grains)
    (not (rock (dnr sand))) (recur (dnr sand) rock grains)
    :else
    (recur sand-source (conj rock sand) (inc grains))))

;;; ex 14-1: 24
(fall-abyss sand-source example 0)

;;; ans 14-1: 774
(fall-abyss sand-source input 0)

(defn floor [rock]
  (->> rock (map :y) (apply max) (+ 2) dec))

(def example-floor (floor example))
(def input-floor (floor input))

(defn fall-floor [sand rock grains floor]
  (cond
    (= (:y sand) floor) (recur sand-source (conj rock sand) (inc grains) floor)
    (not (rock (dwn sand))) (recur (dwn sand) rock grains floor)
    (not (rock (dnl sand))) (recur (dnl sand) rock grains floor)
    (not (rock (dnr sand))) (recur (dnr sand) rock grains floor)
    (= sand sand-source) (inc grains)
    :else
    (recur sand-source (conj rock sand) (inc grains) floor)))

;;; ex 14-2: 93
(fall-floor sand-source example 0 example-floor)

;;; ans 14-2: 22499
(fall-floor sand-source input 0 input-floor)
