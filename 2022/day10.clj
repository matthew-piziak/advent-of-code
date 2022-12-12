(ns aoc.day10
    (:require [aoc.utils :as u]
              [clojure.string :as s]))

(defn parse-cmd
  ([cmd] [(keyword cmd)])
  ([cmd arg] [(keyword cmd) (Integer/parseInt arg)]))

(defn parse-cmds [in]
  (->> in u/read-lines (map #(s/split % #" ")) (map (partial apply parse-cmd))))

(defn execute [cmds]
  (loop [xs (transient [])
         x 1
         [[cmd arg] & rest] cmds]
    (if-not cmd
      (persistent! (conj! xs x))
      (case cmd
        :noop (recur (conj! xs x) x rest)
        :addx (recur (conj! (conj! xs x) x) (+ x arg) rest)))))

(defn signal-strength [in]
  (let [xs (execute (parse-cmds in))]
    (->> (map * xs (range 1 (count xs))) (drop 19) (take-nth 40) (reduce +))))

;;; ex10-1: 13140
(signal-strength "large10")

;;; ans10-1: 13180
(signal-strength "input10")

(defn pixel [crt sprite]
  (if (<= (Math/abs (- crt sprite)) 1) \# \.))

(def scan-length 40)

(defn scan [pixels]
  (partition scan-length (map pixel (cycle (range 0 scan-length)) pixels)))

(defn render [rows]
  (->> rows (map s/join) (map println) doall))

;;; ans 10-2: spells out EZFCHJAB
(->> "input10" parse-cmds execute scan render)
