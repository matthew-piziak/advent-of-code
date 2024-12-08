(ns aoc.day08
  (:require [aoc.grid :as g]
            [clojure.math.combinatorics :as combo]))

(defn antenna-locs [grid]
  (reduce-kv (fn [m k v] (merge-with (partial apply merge) m {v #{k}})) {} grid))

(defn freqs [antenna-locs]
  (keys (dissoc antenna-locs \.)))

(defn antipodes
  ([grid antenna-locs freq]
   (->> (combo/combinations (antenna-locs freq) 2) (mapcat antipodes) (filter grid) set))
  ([p] (let [d (apply (partial mapv -) p)]
         [(mapv + (first p) d) (mapv - (second p) d)])))

(defn uniq-antipodes [in]
  (let [grid (g/read-grid in)
        antenna-locs (antenna-locs grid)]
    (->> antenna-locs freqs (mapcat (partial antipodes grid antenna-locs)) set count)))

(uniq-antipodes "example08")            ; ex 08-1: 14
(uniq-antipodes "input08")              ; an 08-1: 371

(defn slope [p]
  (let [d (apply (partial mapv -) p)
        r (rationalize (apply / d))]
    (if (int? r) [r 1] [(numerator r) (denominator r)])))

(defn antinodes-for-pair [grid p]
  (let [slope (slope p)
        pos-nodes (take-while grid (iterate (partial mapv + slope) (first p)))
        neg-nodes (take-while grid (iterate (partial mapv #(- %2 %1) slope) (first p)))]
    (set (map (partial map long) (concat pos-nodes neg-nodes [(first p)])))))

(defn antinodes-for-freq [grid freq]
  (let [ps (combo/combinations ((antenna-locs grid) freq) 2)]
    (set (mapcat (partial antinodes-for-pair grid) ps))))

(defn uniq-antinodes [in]
  (let [grid (g/read-grid in)]
    (count (set (mapcat (partial antinodes-for-freq grid) (freqs (antenna-locs grid)))))))

(uniq-antinodes "example08")            ; ex 08-2: 34
(uniq-antinodes "input08")              ; an 08-2: 1229
