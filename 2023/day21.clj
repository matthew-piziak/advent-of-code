(ns aoc.day21
  (:require [aoc.utils :as u]
            [instaparse.core :as p]
            [clojure.pprint :as pp]
            [aoc.grid :as g]
            [fastmath.interpolation :as interp]))

(defn start-loc [garden]
  (ffirst (filter (fn [[_ v]] (= v \S)) garden)))

(defn orthos [[x y]]
  (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]))

(defn neighbors [[x y] garden]
  (into #{} (filter (fn [l] (contains? #{\. \S} (garden l))) (orthos [x x]))))

(defn dests
  ([garden steps] (dests garden #{(start-loc garden)} steps))
  ([garden locs steps]
   (if (zero? steps) (count locs)
       (recur garden (into #{} (mapcat #(neighbors % garden) locs)) (dec steps)))))

;; ans 21-1: 16
(def example-garden (g/coords-map (u/read-resource "example21")))
(dests example-garden 6)

;; ans 21-1: 3660
(def input-garden (g/coords-map (u/read-resource "input21")))
(dests input-garden 64)

(defn neighbors-wrapping [[x y] garden size]
  (into #{} (filter (fn [[xx yy]] (contains? #{\. \S} (garden [(mod xx size) (mod yy size)]))) (orthos [x y]))))

(defn dests-wrapping
  ([garden size steps] (dests-wrapping garden size #{(start-loc garden)} steps))
  ([garden size locs steps]
   (if (zero? steps) (count locs)
       (recur garden size (into #{} (mapcat #(neighbors-wrapping % garden size) locs)) (dec steps)))))

(def dests-polynomial
  (let [xs [65 196 327]
        ys (map (partial dests-wrapping input-garden 131) xs)]
    (interp/polynomial xs ys)))

;; ans 21-2: 605492675373144
(dests-polynomial 26501365)
