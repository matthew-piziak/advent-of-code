(ns aoc.day05
  (:require [aoc.utils :as u]
            [clojure.set :as set]))

(defn parse-deps [in]
  (->> in u/read-paras first
     (map (fn [r] (let [[p q] (re-seq #"\d+" r)] {(Integer/parseInt q) #{(Integer/parseInt p)}})))
     (apply (partial merge-with set/union))))

(defn parse-updates [in]
  (map (fn [l] (map #(Integer/parseInt %) (re-seq #"\d+" l))) (second (u/read-paras in))))

(defn relevant-deps [deps update]
  (into {} (map (fn [[k v]] [k (set/intersection v (set update))]) deps)))

(defn ordered?
  ([deps update] (ordered? (relevant-deps deps update) update #{}))
  ([deps update so-far]
   (cond
     (empty? update) true
     (seq (set/difference (deps (first update)) so-far)) false
     :else (recur deps (rest update) (conj so-far (first update))))))

(defn middlesum [ls]
  (transduce (map #(nth %1 (quot (count %1) 2))) + 0 ls))

(defn ordered-middles [in]
  (let [deps (parse-deps in)]
    (->> in parse-updates (filter (partial ordered? deps)) middlesum)))

(ordered-middles "example05")           ; ex 05-1: 143
(ordered-middles "input05")             ; an 05-2: 6267

(defn reorder
  ([deps update] (reorder (relevant-deps deps update) update [] []))
  ([deps update so-far hold]
   (cond
     (seq update) (let [diff (seq (set/difference (deps (first update)) (set so-far)))]
                    (if (seq diff) (recur deps (rest update) so-far (conj hold (first update)))
                        (recur deps (rest update) (conj so-far (first update)) hold)))
     (seq hold) (recur deps hold so-far [])
     :else so-far)))

(defn disordered-middles [in]
  (let [deps (parse-deps in)]
    (middlesum (map (partial reorder deps )(filter (complement (partial ordered? deps)) (parse-updates in))))))

(disordered-middles "example05")        ; ex 05-2: 123
(disordered-middles "input05")          ; an 05-2: 5184
