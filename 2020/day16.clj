(ns aoc.day16
  (:require [aoc.utils :as u]))

(defn valid? [min1 max1 min2 max2]
  (fn [n] (or (<= min1 n max1) (<= min2 n max2))))

(defn checks [conds]
  (map (fn [c] (apply valid? (into [] (map #(Long/parseLong %) (re-seq #"\d+" c))))) conds))

(defn fields [nearby-tickets]
  (map #(Long/parseLong %) (flatten (map (partial re-seq #"\d+") (rest nearby-tickets)))))

(defn tickets [nearby-tickets]
  (partition (count (u/uncommas (nth nearby-tickets 1))) (fields nearby-tickets)))

(defn invalid-fields [conds nearby-tickets]
  (let [tickets (fields nearby-tickets)]
    (remove (apply some-fn (checks conds)) tickets)))

;;; ex 16-1: 71
(let [[conds _ nearby-tickets] (u/read-paras "example16")]
  (apply + (invalid-fields conds nearby-tickets)))

;;; ans 16-1: 28873
(let [[conds _ nearby-tickets] (u/read-paras "input16")]
  (apply + (invalid-fields conds nearby-tickets)))

(defn valid-tickets [invalid-fields nearby-tickets]
  (remove #(some (set invalid-fields) %) (tickets nearby-tickets)))

(defn valid-pairs [in]
  (let [[conds _ nearby-tickets] (u/read-paras in)
        invalid-fields (invalid-fields conds nearby-tickets)]
    (for [cond (range 0 (count conds))
          col (range 0 (count conds))
          :when (every?
                 (nth (checks conds) cond)
                 (map #(nth % col) (valid-tickets invalid-fields nearby-tickets)))]
      [cond col])))

(def valid-cond-to-field (valid-pairs "input16"))

(def sorted-by-count (sort-by count (vals (group-by first valid-cond-to-field))))

(defn rdx [[acc rem]]
  (let [[cond col] (first (first rem))]
    [(conj acc [cond col]) (map #(remove (fn [p] (= col (last p))) %) (rest rem))]))

(def valid-cols (first (last (take 21 (iterate rdx [[] sorted-by-count])))))

(defn your-ticket [in]
  (let [[_ your-ticket _] (u/read-paras in)] (into [] (map #(Long/parseLong %) (u/uncommas (second your-ticket))))))

(your-ticket "input16")

(def departure-cols (filter #(< (first %) 6) valid-cols))

;; ans 16-1 2587271823407
(apply * (map #((your-ticket "input16") %) (map second departure-cols)))
