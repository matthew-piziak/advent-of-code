(ns aoc.day08
  (:require [aoc.utils :as u]))

(def example (u/read-lines "example08"))

(def input (u/read-lines "input08"))

(defn parse-instruction [i]
  (let [[_ op arg] (re-find #"(nop|acc|jmp) ([\+\-]\d+)" i)]
    [op (Integer/parseInt arg)]))

(def example-ins (into [] (map parse-instruction example)))

(def input-ins (into [] (map parse-instruction input)))

(defn exec-ins [ins [n acc]]
  (let [[op arg] (nth ins n)]
    (case op
      "nop" [(inc n) acc]
      "acc" [(inc n) (+ acc arg)]
      "jmp" [(+ n arg) acc])))

(defn exec-until-cycle [ins [n acc] seen]
  (cond
    (contains? seen n) acc
    (= (count ins) n) acc
    :else (exec-until-cycle ins (exec-ins ins [n acc]) (apply merge #{n} seen))))

;; ans 08-1: 2025
(exec-until-cycle input-ins [0 0] #{})

(defn swap-at [n ins]
  (let [[op arg] (nth ins n)]
    (assoc (into [] ins) n [(case op "nop" "jmp" "jmp" "nop" "acc") arg])))

(defn alt-ins [ins]
  (map (fn [n] (swap-at n ins)) (map (fn [[n _]] n) (filter (fn [[_ [op _]]] (contains? #{"jmp" "nop"} op)) (map-indexed vector ins)))))

(defn exec [ins [n acc] seen]
  (cond
    (contains? seen n) :cycle
    (= (count ins) n) acc
    :else (exec ins (exec-ins ins [n acc]) (apply merge #{n} seen))))

;; ans 08-2: 2001
(map (fn [alt] (exec alt [0 0] #{})) (alt-ins input-ins))
