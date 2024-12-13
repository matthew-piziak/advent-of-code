(ns aoc.day12
  (:require [aoc.utils :as u]
            [clojure.core.matrix :as mat]
            [aoc.grid :as g]
            [aoc.flood :as f]))

(defn read-grid [in] (mat/matrix (mapv seq (u/read-lines in))))
(defn coords [m p] (->> m (mat/emap-indexed (fn [[x y] i] [[y x] i])) (apply concat) (filter p)))
(defn region [flooded fill] (set (map first (coords flooded (fn [[_ v]] (= v fill))))))

(defn region-coordinates [grid start]
  (let [fill (gensym) flooded (f/flood-fill grid start fill) region (region flooded fill)]
    [region flooded]))

(defn next-start [flooded] (ffirst (coords flooded (fn [[_ v]] (= java.lang.Character (type v))))))
(defn fences [region plot] (- 4 (count (filter region (g/mh-neighborhood plot)))))
(defn area [region] (count region))
(defn perimeter [region] (transduce (map (partial fences region)) + 0 region))
(defn price [region] (* (area region) (perimeter region)))

(defn total
  ([price-f in] (total price-f (read-grid in) 0))
  ([price-f grid price-acc]
   (let [start (next-start grid)]
     (if (nil? start) price-acc
         (let [[region flooded] (region-coordinates grid start)]
           (recur price-f flooded (+ price-acc (price-f region))))))))

(defn total-price [in] (total price in))

(total-price "example12")               ; ex 12-1: 140
(total-price "large12")                 ; lg 12-1: 1930
(total-price "input12")                 ; in 12-1: 1467094

(defn corners [[x y]]
  (let [ul [[(dec x) y] [(dec x) (dec y)] [x (dec y)]] ur [[x (dec y)] [(inc x) (dec y)] [(inc x) y]]
        lr [[(inc x) y] [(inc x) (inc y)] [x (inc y)]] ll [[x (inc y)] [(dec x) (inc y)] [(dec x) y]]]
    [ul ur lr ll]))

(defn count-corners [mask region plot]
  (count (filter (fn [c] (= mask (map (partial contains? region) c))) (corners plot))))

(def concave-corners (partial count-corners '(false false false)))
(def convex-corners (partial count-corners '(true false true)))
(def bridge-corners (partial count-corners '(false true false)))
(defn plot-corners [region plot] (apply + ((juxt concave-corners convex-corners bridge-corners) region plot)))
(defn region-corners [region] (transduce (map (partial plot-corners region)) + 0 region))
(defn discounted [region] (* (area region) (region-corners region)))
(defn total-discounted [in] (total discounted in))

(total-discounted "example12")          ; ex 12-2: 80
(total-discounted "large12")            ; lg 12-2: 1206
(total-discounted "input12")            ; in 12-2: 881182
