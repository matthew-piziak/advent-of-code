(ns aoc.day22
  (:require [aoc.utils :as u]
            [instaparse.core :as p]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.core.logic.fd :as fd]))

(defn parse-brick [b]
  (let [parse (p/parser "brick = end <'~'> end
                         end = num <','> num <','> num
                         num = #'[0-9]+'")
        transform {:num (fn [n] (Long/parseLong n))
                   :end (fn [x y z] {:x x :y y :z z})
                   :brick (fn [e1 e2] (merge-with (fn [start end] [start end]) e1 e2))}]
    (->> b parse (p/transform transform))))

(defn parse-bricks [in]
  (->> in u/read-lines (map-indexed (fn [i l] (assoc (parse-brick l) :idx i)))))

(def example-bricks (parse-bricks "example22"))
(def input-bricks (parse-bricks "input22"))

(defn lower [brick] (update brick :z (partial mapv dec)))
(defn bot [brick] (first (:z brick)))
(defn top [brick] (second (:z brick)))

(defn disjoint? [b1 b2]
  (letfn [(overlaps? [d] (fd/disjoint?* (apply fd/interval (d b1)) (apply fd/interval (d b2))))]
    (or (overlaps? :x) (overlaps? :y))))

(defn all-disjoint? [brick bricks]
  (every? (partial disjoint? brick) bricks))

(defn supporters [brick bricks]
  (map :idx (remove (partial disjoint? brick) bricks)))

(defn settle
  ([bricks] (settle [] {} bricks))
  ([settled foundation bricks]
   (if (empty? bricks) settled
       (let [brick (first bricks)]
         (if (= 1 (bot brick))
           (settle (conj settled (assoc brick :supported-by (list :ground)))
                   (update foundation (top brick) #(conj % brick))
                   (rest bricks))
           (let [lowered (lower brick)]
             (cond
               (not (foundation (bot lowered))) (recur settled foundation (conj (rest bricks) lowered))
               (all-disjoint? brick (foundation (bot lowered))) (recur settled foundation (conj (rest bricks) lowered))
               :else
               (recur (conj settled (assoc brick :supported-by (supporters brick (foundation (bot lowered)))))
                      (update foundation (top brick) #(conj % brick))
                      (rest bricks)))))))))

(defn ->supported-by [bricks]
  (apply merge (map (fn [b] {(:idx b) (or (:supported-by b) (list :ground))}) (settle (sort-by bot bricks)))))

(defn ->supports [supported-by]
  (apply (partial merge-with concat) (flatten (map (fn [[k vs]] (map (fn [v] {v (list k)}) vs)) supported-by))))

(defn drops [supports supported-by idx]
  (filter (fn [supported] (= 1 (count (supported-by supported)))) (supports idx)))

(defn safe? [supports supports-by idx]
  (empty? (drops supports supports-by idx)))

(defn count-safe [bricks]
  (let [supported-by (->supported-by bricks)
        supports (->supports supported-by)]
    (count (filter (fn [b] (safe? supports supported-by (:idx b))) bricks))))

;;; ex 22-1: 5
(count-safe example-bricks)

;;; ans 22-1: 512
(count-safe input-bricks)

(defn collapse
  ([supports supported-by idx] (collapse supports supported-by (u/queue [idx]) #{idx}))
  ([supports supported-by dropping dropped]
   (cond
     (empty? dropping) dropped
     (safe? supports supported-by (peek dropping))
     (recur (dissoc supports (peek dropping))
            (into {} (map (fn [[k v]] [k (remove #{(peek dropping)} v)]) supported-by))
            (pop dropping)
            (conj dropped (peek dropping)))
     :else
     (let [ds (drops supports supported-by (peek dropping))]
       (recur
        (dissoc supports (peek dropping))
        (into {} (map (fn [[k v]] [k (remove #{(peek dropping)} v)]) supported-by))
        (reduce (fn [q t] (conj q t)) (pop dropping) ds) (apply conj dropped ds))))))

(defn total-collapse [bricks]
  (let [supported-by (->supported-by bricks)
        supports (->supports supported-by)]
    (apply + (map (fn [brick] (dec (count (collapse supports supported-by (:idx brick))))) bricks))))

;;; ex 22-2: 7
(total-collapse example-bricks)

;;; ans 22-2: 98167
(total-collapse input-bricks)
