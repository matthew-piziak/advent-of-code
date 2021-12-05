(ns aoc.day05
  (:require [aoc.utils :as u]
            [clojure.core.matrix :as mat]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

(def example (u/read-lines "example05"))

(defn line->coords [line]
  (let [[x1 y1 x2 y2] (mapv #(Long/parseLong %) (re-seq #"\d+" line))]
    {:x1 x1 :y1 y1 :x2 x2 :y2 y2}))

(defn coords [in]
  (map line->coords in))

(defn horizontal? [{y1 :y1 y2 :y2}]
  (= y1 y2))

(defn vertical? [{x1 :x1 x2 :x2}]
  (= x1 x2))

(defn orthogonal? [coords]
  (or (horizontal? coords) (vertical? coords)))

(defn lerp [coords]
  (cond
    (horizontal? coords) (let [{y1 :y1 x1 :x1 x2 :x2} coords]
                           (map (fn [x] {:x x :y y1}) (range (min x1 x2) (inc (max x1 x2)))))
    (vertical? coords) (let [{x1 :x1 y1 :y1 y2 :y2} coords]
                         (map (fn [y] {:y y :x x1}) (range (min y1 y2) (inc (max y1 y2)))))
    :else (let [{x1 :x1 x2 :x2 y1 :y1 y2 :y2} coords]
            (map (fn [x y] {:x x :y y})
                 (range x1 ((if (<= x1 x2) inc dec) x2) (if (<= x1 x2) 1 -1))
                 (range y1 ((if (<= y1 y2) inc dec) y2) (if (<= y1 y2) 1 -1))))))

(defn overlaps [in]
  (count (filter (fn [[_ v]] (> v 1)) (frequencies (flatten (map lerp (filter orthogonal? (coords in))))))))

;;; ex05-1: 5
(overlaps example)

(def input (u/read-lines "input05"))

;;; ans05-1: 6548
(overlaps input)

(defn overlaps-diag [in]
  (count (filter (fn [[_ v]] (> v 1)) (frequencies (flatten (map lerp (coords in)))))))

;;; ex05-2: 12
(overlaps-diag example)

;;; ans05-2: 19663
(overlaps-diag input)
