(ns aoc.day03
    (:require [aoc.utils :as u]
              [instaparse.core :as p]
              [instaparse.gll :as gll]))

(defn read-schematic [input]
  (let [schematic (u/read-resource input)
        parse (p/parser "schematic = (<space> (num / symbol))* <space>
                         num = #'[0-9]+'
                         symbol = #'[\\!-\\-\\@\\/\\=]'
                         space = #'[\\.\\n]*'")
        transform {:num (fn [n] [:num (Integer/parseInt n)])
                   :schematic (fn [& elements] elements)}]
    (->> schematic parse (p/add-line-and-column-info-to-metadata schematic) (p/transform transform))))

(defn symb-idxs [symbs]
  (letfn [(adjs [[x y]] (for [dx [-1 0 1] dy [-1 0 1]] [(+ x dx) (+ y dy)]))]
    (->> symbs (mapcat (comp adjs (juxt ::gll/start-line ::gll/start-column) meta)) (into #{}))))

(defn idxs [e]
  (let [m (meta e)]
    (into [] (map (fn [c] [(::gll/start-line m) c]) (range (::gll/start-column m) (::gll/end-column m))))))

(defn part? [num symb-idxs]
  (->> num num-idxs (some symb-idxs)))

(defn sum-parts [input]
  (let [schematic (read-schematic input)
        symb-idxs (symb-idxs (filter #(= :symbol (first %)) schematic))]
    (->> schematic (filter #(= :num (first %))) (filter #(part? % symb-idxs)) (map second) (apply +))))

;;; ex 03-01: 4361
(sum-parts "example03")

;;; ans 03-01: 535235
(sum-parts "input03")

(defn gear-ratio [nums star]
  (let [gear-parts (filter #(some (symb-idxs [star]) %) (map (comp (partial into #{}) idxs) nums))]
    (if (= 2 (count gear-parts)) (->> nums (filter #(part? % (symb-idxs [star]))) (map second) (apply *)) 0)))

(defn gear-sum [input]
  (let [schematic (read-schematic input)
        stars (filter (fn [[k s]] (= [k s] [:symbol "*"])) schematic)
        nums (filter (fn [[k _]] (= k :num)) schematic)]
    (->> stars (map #(gear-ratio nums %)) (apply +))))

;; ex 03-2: 467835
(gear-sum "example03")

;; ans 03-2: 79844424
(gear-sum "input03")
