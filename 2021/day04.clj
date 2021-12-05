(ns aoc.day04
  (:require [aoc.utils :as u]
            [clojure.core.matrix :as mat]
            [clojure.set :as set]))

(def example (u/read-paras "example04"))

(defn nums [in]
  (map #(Long/parseLong %) (re-seq #"\d+" (ffirst in))))

(defn boards [in]
  (into [] (rest in)))

(defn board->matrix [board]
  (mat/matrix (map (fn [r] (map (fn [n] (Long/parseLong n)) (re-seq #"\d+" r))) board)))

(defn matrix->lines [m]
  (concat (mat/columns m) (mat/rows m)))

(defn lines [in]
  (->> in
     boards
     (map board->matrix)
     (map-indexed (fn [i m] (map (fn [r] {(set r) i}) (matrix->lines m))))
     (apply concat)
     (apply merge)))

;;; return index of winning board, and marked numbers
(defn winning-board [nums rem lines]
  (let [ns (set nums)
        winners (filter (fn [l] (set/subset? l ns)) (keys lines))]
    (if (not (empty? winners))
      [(lines (first winners)) nums]
      (recur (conj nums (first rem)) (rest rem) lines))))

(defn score [in]
  (let [[windex marked] (winning-board [] (nums in) (lines in))
        ss (into #{} (apply concat (board->matrix (get (boards in) windex))))]
    (* (apply + (set/difference ss (into #{} marked))) (last marked))))

;;; ex04-1: 4512
(score example)

(def input (u/read-paras "input04"))

;;; ans04-1: 33462
(score input)

;;; return index of losing board, and marked numbers
(defn losing-board [nums rem lines cand]
  (let [ns (set nums)
        winners (filter (fn [l] (set/subset? l ns)) (keys lines))
        losing-boards (set/difference (into #{} (vals lines)) (into #{} (map #(lines %) winners)))]
    (case (count losing-boards)
      0 [cand nums]
      1 (recur (conj nums (first rem)) (rest rem) lines (first losing-boards))
      (recur (conj nums (first rem)) (rest rem) lines cand))))

(defn score-loser [in]
  (let [[lindex marked] (losing-board [] (nums in) (lines in) nil)
        ss (into #{} (apply concat (board->matrix (get (boards in) lindex))))]
    (* (apply + (set/difference ss (into #{} marked))) (last marked))))

;;; ex04-2: 1924
(score-loser example)

;;; ans04-2: 30070
(score-loser input)
