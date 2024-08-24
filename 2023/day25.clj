(ns aoc.day25
  (:require
   [aoc.utils :as u]
   [instaparse.core :as p]
   [multiset.core :as ms]))

(defn parse-line [l]
  (let [parse (p/parser "line = component <': '> component (<' '> component)*
                         component = #'[a-z]{3}'")
        transform {:component (fn [c] c)
                   :line (fn [o & ds] (map (fn [d] #{o d}) ds))}]
    (->> l parse (p/transform transform))))

(defn parse-diagram [in]
  (->> in u/read-lines (map parse-line) (apply concat) (apply ms/multiset)))

(defn choose-edge [g]
  (rand-nth (vec g)))

(defn contract [g e]
  (let [c (apply str e)
        [o d] (vec e)]
    (->> g
         (remove #(= e %))
         (map (comp set (partial replace {o c d c})))
         (apply ms/multiset))))

(defn cut [g]
  (let [e (choose-edge g)
        gg (contract g e)
        k (count (keys (ms/multiplicities gg)))]
    (if (>= 1 k) (-> gg ms/multiplicities first)
      (recur gg))))

(defn loop-cut [g]
  (loop [[e mult] (cut g)]
    (if (= 3 mult) (* (/ (count (first e)) 3) (/ (count (second e)) 3))
        (recur (cut g)))))

;;; ex 25: 54
(loop-cut (parse-diagram "example25"))

;; ans 25: 544523
(loop-cut (parse-diagram "input25"))
