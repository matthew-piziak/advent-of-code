(ns aoc.day01
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn cal [line]
  (let [digits (re-seq #"\d" line)]
    (str (first digits) (last digits))))

(defn sum-lines [input f]
  (transduce (map f) + 0 (u/read-lines input)))

(defn part-one [input]
  (sum-lines input (comp #(Integer/parseInt %) cal)))

;;; ex 01-1: 142
(part-one "example01")

;;; ans 01-1: 56465
(part-one "input01")

;;; thanks, Common Lisp
(def to-english (partial clojure.pprint/cl-format nil "~R"))

(def digit-words (into {} (map (juxt to-english identity) (range 1 10))))

(defn re-disj [ms]
  (re-pattern (s/join "|" ms)))

(def re-digit (re-disj (conj (keys digit-words) "\\d")))

;;; avoid overlapping matches by just reversing the regex and reading from the end
(def re-digit-reversed (re-disj (conj (map s/reverse (keys digit-words)) "\\d")))

(defn cal2 [line]
  (let [f (re-find re-digit line)
        l (s/reverse (re-find re-digit-reversed (s/reverse line)))]
    (str (or (digit-words f) f) (or (digit-words l) l))))

(defn part-two [input]
  (sum-lines input (comp #(Integer/parseInt %) cal2)))

;;; ex 01-2: 281
(part-two "large01")

;;; ans 01-2: 55902
(part-two "input01")
