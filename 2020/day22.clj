(ns aoc.day22
    (:require [aoc.utils :as u]
              [clojure.pprint :as p]
              [clojure.set :as set]))

(defn p->queue [p]
  (apply (partial conj (clojure.lang.PersistentQueue/EMPTY)) p))

(defn parse-decks [in]
  (let [[p1 p2] (map rest (u/read-paras in))]
    [(p->queue (map #(Integer/parseInt %) p1)) (p->queue (map #(Integer/parseInt %) p2))]))

(defn step [[p1 p2]]
  (let [c1 (peek p1) c2 (peek p2)]
    (if (> c1 c2)
      [(conj (pop p1) c1 c2) (pop p2)]
      [(pop p1) (conj (pop p2) c2 c1)])))

(defn winner [[p1 p2]]
  (if (empty? p1) p2 p1))

(defn combat [in]
  (step (last (take-while (fn [[p1 p2]] (and (seq p1) (seq p2))) (iterate step (parse-decks in))))))

(defn score [winner]
  (apply + (map (fn [x y] (* x y)) winner (range (count winner) 0 -1))))

(defn score-1 [in]
  (let [winner (winner (combat in))]
    (score winner)))

;; ex 22-1: 306
(score-1 "example22")

;; ans 22-1: 35299
(score-1 "input22")

(defn sub-queue [c p]
  (apply (partial conj (clojure.lang.PersistentQueue/EMPTY)) (take c p)))

(defn recursive-combat [[p1 p2] game round seen]
  (if (contains? seen [p1 p2]) [:p1 p1]
    (cond
      (empty? p1) [:p2 p2]
      (empty? p2) [:p1 p1]
      :else
      (let [c1 (peek p1) c2 (peek p2)
            r1 (pop p1) r2 (pop p2)]
        (if (and (<= c1 (count r1)) (<= c2 (count r2)))
          (let [rc (do
                     (when (or (= game 1) (and (< 1000 round) (zero? (mod round 100))))
                       (p/pprint (format "-- Round %d (Game %d)--" round game))
                       (Thread/sleep 10))
                     (recursive-combat [(sub-queue c1 r1) (sub-queue c2 r2)] (inc game) 1 #{}))]
            (cond
              (= :p1 (first rc)) (recur [(conj r1 c1 c2) r2] game (inc round) (conj seen [p1 p2]))
              (= :p2 (first rc)) (recur [r1 (conj r2 c2 c1)] game (inc round) (conj seen [p1 p2]))))
          (recur (step [p1 p2]) game (inc round) (conj seen [p1 p2])))))))

;;; ans 22-2: 33266
(score (nth (recursive-combat (parse-decks "input22") 1 1 #{}) 1))
