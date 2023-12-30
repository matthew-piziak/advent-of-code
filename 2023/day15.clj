(ns aoc.day15
    (:require [aoc.utils :as u]
              [clojure.string :as s]))

(defn hhash [s]
  (reduce (fn [curr c] (-> curr (+ (int c)) (* 17) (rem 256))) 0 s))

;;; ex 15-HASH: 52
(hhash "HASH")

(def example (s/split "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" #","))

;;; ex 15-1: 1320
(transduce (map hhash) + 0 example)

(def input (-> "input15" u/read-resource s/trim-newline (s/split #",")))

;; ans 15-1: 519603
(transduce (map hhash) + 0 input)

(defn parse [inst]
  (if (s/ends-with? inst "-") [:rem (apply str (butlast inst))]
      (let [[l f] (s/split inst #"=")] [:add l (Integer/parseInt f)])))

(defn add-lens [ls l f]
  (cond
    (nil? ls) [[l f]]
    (some #{l} (map first ls)) (mapv (fn [[ll ff]] (if (= ll l) [l f] [ll ff])) ls)
    :else (conj ls [l f])))

(defn score [[h ls]]
  (apply + (map-indexed (fn [i [l f]] (* (inc h) (inc i) f)) ls)))

(defn power [in]
  (transduce (map score) + 0
   (loop [insts (map parse in)
          boxes {}]
     (if (empty? insts) boxes
         (let [[i & is] insts]
           (case (first i)
             :add (let [[_ l f] i] (recur is (update boxes (hhash l) (fn [o] (add-lens o l f)))))
             :rem (let [[_ l] i] (recur is (update boxes (hhash l) #(into [] (remove (fn [[ll _]] (= ll l)) %)))))))))))

;;; ex 15-2: 145
(power example)

;;; ans 15-2: 244342
(power input)
