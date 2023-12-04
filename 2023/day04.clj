(ns aoc.day04
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn ->ints [s]
  (set (map #(Integer/parseInt %) (re-seq #"\d+" s))))

(defn ->card [line]
  (let [[winners my] (s/split (second (s/split line #"\:")) #"\|")]
    {:winners (->ints winners) :my (->ints my) :copies 1}))

(defn matches [card]
  (count (set/intersection (:winners card) (:my card))))

(defn score [matches]
  (if (zero? matches) 0 (int (.pow (BigInteger. "2") (dec matches)))))

;;; ex 04-1: 13
(u/sum-lines "example04" (map (comp score matches ->card)))

;;; ans 04-1: 23750
(u/sum-lines "input04" (map (comp score matches ->card)))

(defn count-cards
  ([input] (count-cards (map ->card (u/read-lines input)) 0))
  ([deck n]
   (if (empty? deck) n
       (let [[card & remaining] deck
             matches (matches card)
             copies (:copies card)
             awarded (map #(update-in % [:copies] + copies) (take matches remaining))]
         (recur (concat awarded (drop matches remaining)) (+ n copies))))))

;;; ex 04-2: 30
(count-cards "example04")

;;; ans 04-2: 13261850
(count-cards "input04")
