(ns aoc.day04
    (:require [aoc.utils :as u]
              [clojure.string :as s]))

(def input (u/read-paras "input04"))

(defn valid? [passport]
  (and
   (s/includes? passport "byr:")
   (s/includes? passport "iyr:")
   (s/includes? passport "eyr:")
   (s/includes? passport "hgt:")
   (s/includes? passport "hcl:")
   (s/includes? passport "ecl:")
   (s/includes? passport "pid:")))

;;; ans-04-1: 192
(count (filter #(valid? (s/join " " %)) input))

(defn parse-field [field]
  (into [] (rest (first (re-seq #"([a-z]{3}):([a-zA-Z0-9#]+)" field)))))

(defn valid-field? [[field val]]
  (case field
    "byr" (<= 1920 (Integer/parseInt (or val "0")) 2002)
    "iyr" (<= 2010 (Integer/parseInt (or val "0")) 2020)
    "eyr" (<= 2020 (Integer/parseInt (or val "0")) 2030)
    "ecl" (or (= "amb" val) (= "blu" val) (= "brn" val) (= "gry" val) (= "grn" val) (= "hzl" val) (= "oth" val))
    "pid" (re-matches #"\d{9}" val)
    "hcl" (re-matches #"#[0-9a-f]{6}" val)
    "hgt" (or
           (<=  150 (Integer/parseInt (nth (re-matches #"(\d{3})cm" val) 1 "0")) 193)
           (<=  59 (Integer/parseInt (nth (re-matches #"(\d{2})in" val) 1 "0")) 76))
    "cid" true))

;;; ans-04-2: 101
(->> input
   (filter #(valid? (s/join " " %)))
   (map #(map (fn [i] (parse-field i)) %))
   (map #(list %1 (every? valid-field? %1)))
   (filter #(nth % 1))
   count)
