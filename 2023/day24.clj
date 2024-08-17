(ns clojure-solutions.day24
  (:require
   [aoc.utils :as u]
   [clojure.core.matrix :as mat]
   [clojure.core.matrix.operators :refer [+ -]]
   [clojure.string :as s]
   [instaparse.core :as p]))

(defn parse-hailstone [l]
  (let [[px py pz vx vy vz] (->> l (re-seq #"-*\d+") (map read-string))]
    {:px px :py py :pz pz :vx vx :vy vy :vz vz}))

(defn parse-hailstones [in]
  (->> (u/read-lines in) (map parse-hailstone)))

(defn p [hs] ((juxt :px :py :pz) hs))
(defn v [hs] ((juxt :vx :vy :vz) hs))

(defn intersection [hs1 hs2]
  (let [denom (- (* (:vx hs1) (:vy hs2)) (* (:vy hs1) (:vx hs2)))]
    (when-not (zero? denom)
      (let [nom (+ (* (:vy hs1) (- (:px hs2) (:px hs1))) (* (:vx hs1) (- (:py hs1) (:py hs2))))]
        (/ nom denom)))))

(defn crossers [hss]
  (let [min 200000000000000 max 400000000000000
        between? (fn [n] (<= min n max))
        valid? (fn [p v i] (between? (+ p (* v i))))]
    (count
     (for [n (range (count hss))
           m (range n (count hss))
           :let [hs1 (nth hss n) hs2 (nth hss m) i1  (intersection hs1 hs2) i2 (intersection hs2 hs1)]
           :when (and i1 (pos? i1) (pos? i2) (valid? (:px hs2) (:vx hs2) i1) (valid? (:py hs2) (:vy hs2) i1))] :hs))))

;; ans 24-1: 13892
(-> "input24" parse-hailstones crossers)

(defn collider [[h1 h2 h3 & _]]
  (letfn [(cross [[x y z]] [[0 (- z) y] [z 0 (- x)] [(- y) x 0 ]])
          (cross-on [f] (comp cross f))
          (mom [h] (mat/cross (p h) (v h)))
          (res [f v1 v2] (let [g (cross-on f)] (- (g v1) (g v2))))]
    (let [moms (vec (concat (- (mom h1) (mom h2)) (- (mom h1) (mom h3))))
          solver (->
                  (concat
                   (mapv (fn [a b] (vec (concat a b))) (res v h2 h1) (res p h2 h1))
                   (mapv (fn [a b] (vec (concat a b))) (res v h3 h1) (res p h3 h1)))
                  vec mat/inverse)]
      (->> (mat/mmul solver moms) (take 3) (reduce +) Math/round long))))

;; ans 24-2: 843888100572888
(collider (parse-hailstones "input24"))
