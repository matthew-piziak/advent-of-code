(ns aoc.day10
  (:require [aoc.utils :as u]
            [aoc.grid :as g]))

(defn read-map [in]
  (->> in g/read-grid (map (fn [[k v]] [k (u/char->int v)])) (into {})))

(defn trailheads [grid]
  (->> grid (filter (fn [[_ v]] (= v 0))) (map first)))

(defn trails
  ([grid trailhead] (trails grid (u/queue [[trailhead]]) []))
  ([grid trails hiking-trails]
   (if (empty? trails) hiking-trails
       (let [trail (peek trails) loc (last trail) height (grid loc)]
         (if (= height 9) (recur grid (pop trails) (conj hiking-trails trail))
             (let [nd (g/mh-neighborhood loc)
                   h (inc height)
                   nexts (->> nd (filter #(and (grid %) (= h (grid %)))))]
               (recur grid (apply conj (pop trails) (map #(conj trail %) nexts)) hiking-trails)))))))

(defn trail-score [grid trailhead]
  (->> trailhead (trails grid) (map last) (into #{}) count))

(defn map-score [grid]
  (transduce (map (partial trail-score grid)) + 0 (trailheads grid)))

(map-score (read-map "example10"))      ; ex 10-1: 1
(map-score (read-map "large10"))        ; lg 10-1: 36
(map-score (read-map "input10"))        ; an 10-1: 430

(defn trail-rating [grid trailhead]
  (->> trailhead (trails grid) count))

(defn map-rating [grid]
  (transduce (map (partial trail-rating grid)) + 0 (trailheads grid)))

(map-rating (read-map "large10"))       ; lg 10-2: 81
(map-rating (read-map "input10"))       ; an 10-2: 928
