(ns aoc.day20
    (:require [aoc.utils :as u]
              [clojure.core.matrix :as m]
              [clojure.java.io :as io]
              [clojure.string :as s]))

(defn tile-edges [tile-lines]
  [(first tile-lines)
   (apply str (map last tile-lines))
   (last tile-lines)
   (apply str (map first tile-lines))])

(defn rotate [tile-edges] (into [] (cons (last tile-edges) (drop-last tile-edges))))
(defn r1 [tile-edges] (rotate tile-edges))
(defn r2 [tile-edges] (rotate (r1 tile-edges)))
(defn r3 [tile-edges] (rotate (r2 tile-edges)))

(defn m1 [[e1 e2 e3 e4]] [e1 e2 e3 e4])

(def transforms [identity r1 r2 r3 m1 (comp m1 r1) (comp m1 r2) (comp m1 r3)])

(defn parse-tile [para]
  (let [[id-line & tile-lines] para]
    {:tile (Long/parseLong (second (re-find #"Tile (\d+):" id-line))), :tile-edges (tile-edges tile-lines)}))

(defn edge-to-binary [edge]
  (Long/parseLong (s/replace (s/replace edge "." "0") "#" "1") 2))

(defn t-m-b [[t _ _ _] [_ _ b _]] (= t b))

;;; all transforms
(->> "example20"
   (u/read-paras)
   (map parse-tile)
   (map :tile-edges)
   (map (fn [es] (map (fn [t] (t es)) transforms)))
   (map (partial map (partial map edge-to-binary))))

(->> "small20"
   (u/read-paras)
   (map parse-tile)
   (map :tile-edges)
   (map (fn [es] (map (fn [t] (t es)) transforms)))
   (first))
