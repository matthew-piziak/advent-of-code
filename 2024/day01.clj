(ns aoc.day01
  (:require [aoc.utils :as u]
            [instaparse.core :as p]))

(defn read-ids [line]
  (let [parse (p/parser "ids = num <ws> num num = #'[0-9]+' ws = #'\\s*'")
        transform {:num #(Integer/parseInt %) :ids (fn [& nums] nums)}]
    (p/transform transform (parse line))))

(defn sum-distances [in]
  (->> in u/read-lines (map read-ids) u/transpose (map sort) u/transpose (map u/absdiff) (apply +)))

(sum-distances "example01")             ; ex 01-1: 11
(sum-distances "input01")               ; an 01-1: 1151792

(defn similarity-score [in]
  (let [[left right] (->> in u/read-lines (map read-ids) u/transpose)
        freqs (frequencies right)]
    (->> left (map #(* %1 (get freqs %1 0))) (apply +))))

(similarity-score "example01")          ; ex 01-2: 31
(similarity-score "input01")            ; an 01-2: 21790168
