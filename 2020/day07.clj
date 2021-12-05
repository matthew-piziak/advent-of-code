(ns aoc.day07
    (:require [aoc.utils :as u]
              [clojure.string :as s]))


(def input (u/read-lines "input07sing"))

(def bags (into #{} (flatten (map #(re-seq #"\w+ \w+ bag" %) input))))

(import 'java.util.regex.Pattern)
(def bags-rx (re-pattern (s/join \| (map #(Pattern/quote %) bags))))

(defn match-bags [l]
  (re-seq bags-rx l))

(defn bag-search [frontier]
  (->> input
     (map match-bags)
     (filter #(some frontier %))
     (map first)
     (into #{})))

(defn bag-contents [bag]
  (first (filter #(= bag (first %)) (map match-bags input))))

(bag-contents "shiny gold bag")

;;; ansa 07-1: 119
(let [initial #{"shiny gold bag"}]
  (->> initial
     (iterate bag-search)
     (take 12)
     (last)
     (remove initial)
     (count)))

(defn parse-contents [[_ n color]]
  (hash-map color (Integer/parseInt (or n "0"))))

(defn parse-line [l]
  (let [[[_ _ container] & contents] (re-seq #"(\d)? ?(\w+ \w+) bag" l)]
    (hash-map container (apply merge (map parse-contents contents)))))

(def contents (apply merge (map parse-line input)))

(defn walk-contents [cs]
  (apply (partial merge-with +) (map (fn [[k v]] (into {} (for [[kk vv] (contents k)] [kk (* v vv)]))) cs)))

;; ans 07-2: 155802
(apply + (rest (map (partial apply +) (map vals (take 10 (iterate walk-contents {"shiny gold" 1}))))))
