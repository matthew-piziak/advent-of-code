(ns aoc.day02
  (:require [aoc.utils :as u]
            [instaparse.core :as p]))

(defn read-game [game]
  (let [parse (p/parser "game = <'Game'> <ws> num <':'> <ws> round (<ws> <';'> <ws> round)*
                         round = colorset (<ws> <','> <ws> colorset)*
                         colorset = num <ws> color
                         color = 'blue' / 'red' / 'green'
                         num = #'[0-9]+'
                         ws = #'\\s*'")
        transform {:num #(Integer/parseInt %)
                   :color (fn [color] (keyword color))
                   :colorset (fn [num color] {color num})
                   :round (fn [& rounds] (apply merge rounds))
                   :game (fn [num & rounds] {:game-id num :rounds rounds})}]
    (p/transform transform (parse game))))

(defn round-possible? [{blue :blue red :red green :green :or {blue 0 red 0 green 0}}]
  (and (<= red 12) (<= green 13) (<= blue 14)))

(defn game-possible? [game]
  (every? round-possible? (:rounds game)))

(defn possible-sum [input]
  (u/sum-lines input (comp (map read-game) (filter game-possible?) (map :game-id))))

;;; ex 02-1: 8
(possible-sum "example02")

;;; ans 02-2: 2105
(possible-sum "input02")

(defn power [game]
  (->> game :rounds (apply (partial merge-with max)) vals (apply *)))

(defn power-sum [input]
  (u/sum-lines input (map (comp power read-game))))

;;; ex 02-2: 2286
(power-sum "example02")

;;; ans 02-2: 72422
(power-sum "input02")
