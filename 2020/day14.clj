(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn and-mask [mask]
  (Long/parseLong (apply str (replace {\X 1 \1 1 \0 0} mask)) 2))

(defn or-mask [mask]
  (Long/parseLong (apply str (replace {\X 0 \1 1 \0 0} mask)) 2))

(defn apply-mask [or-mask and-mask mem [adr val]]
  (assoc mem adr (bit-or or-mask (bit-and val and-mask))))

(defn parse-mask [mask-str]
  (second (re-find #"mask = (\w+)" mask-str)))

(defn parse-asn [asn-str]
  (let [[_ adr val] (re-find #"mem\[(\d+)\] = (\d+)" asn-str)] [(Long/parseLong adr) (Long/parseLong val)]))

(defn parse-para [para]
  [(parse-mask (first para)) (map parse-asn (rest para))])

(defn apply-para [mem para]
  (let [[mask asns] (parse-para para)]
    (reduce (partial apply-mask (or-mask mask) (and-mask mask)) mem asns)))

;; ans 14-1: 10035335144067
(apply + (vals (reduce apply-para {} (u/read-paras "input14"))))

(defn first-mask [mask]
  (apply str (replace {\X 0 \1 1 \0 0} mask)))

(defn x-only [mask]
  (apply str (replace {\X \X \1 \0 \0 \0} mask)))

(defn and-mask-floats [mask]
  (if (s/includes? mask "X")
    (flatten [(and-mask-floats (s/replace-first mask "X" "0")) (and-mask-floats (s/replace-first mask "X" "1"))])
    mask))

(defn adrs [mem mask]
  (map #(bit-xor (bit-or (Long/parseLong (first-mask mask) 2) mem) %)
       (map #(Long/parseLong % 2) (and-mask-floats (x-only mask)))))

(defn apply-mask-2 [mask mem [adr val]]
  (apply (partial assoc mem) (flatten (map (fn [a] [a val]) (adrs adr mask)))))

(defn apply-para-2 [mem para]
  (let [[mask asns] para]
    (reduce (partial apply-mask-2 mask) mem asns)))

(def example-paras (map parse-para (u/read-paras "example14")))
(apply + (vals (reduce apply-para-2 {} example-paras)))

(def input-paras (map parse-para (u/read-paras "input14")))

;; ans 14-2: 3817372618036
(time (apply + (vals (reduce apply-para-2 {} input-paras))))
