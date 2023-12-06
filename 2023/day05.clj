(ns aoc.day05
  (:require [aoc.utils :as u]
            [instaparse.core :as p]))

(defn read-almanac [input]
  (let [almanac (u/read-resource input)
        parse (p/parser
               "almanac = seeds ss sf fw wl lt th hl
                seeds = <ws> <'seeds: '> (num <ws> num <ws>)+ <nl>
                ss = <'seed-to-soil map:'> <nl> almap+
                sf = <'soil-to-fertilizer map:'> <nl> almap+
                fw = <'fertilizer-to-water map:'> <nl> almap+
                wl = <'water-to-light map:'> <nl> almap+
                lt = <'light-to-temperature map:'> <nl> almap+
                th = <'temperature-to-humidity map:'> <nl> almap+
                hl = <'humidity-to-location map:'> <nl> almap*
                almap = num <ws> num <ws> num <nl>
                num = #'[0-9]+'
                ws = #' '*
                nl = #'\\n'+")
        transform {:num (fn [n] [:num (Long/parseLong n)])
                   :almap (fn [[_ d] [_ s] [_ l]] [d s l])
                   :seeds (fn [& seeds] {:seeds (into [] (map second seeds))})
                   :almanac (fn [seeds & ms] (merge seeds (apply merge (map (fn [[k & v]] {k v}) ms))))}]
    (->> almanac parse (p/transform transform))))

(defn mvs [i mps]
  (if (empty? mps) i
      (let [[[d s l] & rem] mps
            j (if (and (>= i s) (< i (+ s l))) (+ i (- d s)) i)]
        (if (not= i j) j (recur j rem)))))

(defn ->loc [almanac seeds]
  (apply min (reduce (fn [ss k] (map (fn [s] (mvs s (k almanac))) ss)) seeds [:ss :sf :fw :wl :lt :th :hl])))

;;; ex 05-1: 35
(let [almanac (read-almanac "example05")]
  (->loc almanac (:seeds almanac)))

;; ans 05-1: 88151870
(let [almanac (read-almanac "input05")]
  (->loc almanac (:seeds almanac)))

;;; ex 05-1: 46
(let [almanac (read-almanac "example05")]
  (apply min (pmap (fn [[d l]] (->loc almanac (range d (+ d l)))) (partition 2 (:seeds almanac)))))

;;; ans 05-2: 2008785
(let [almanac (read-almanac "input05")]
  (apply min (pmap (fn [[d l]] (->loc almanac (range d (+ d l)))) (partition 2 (:seeds almanac)))))
