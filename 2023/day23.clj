(ns aoc.day23
  (:require [aoc.grid :as g]
            [aoc.utils :as u]
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn start-end [in]
  (let [lines (u/read-lines in)]
    (map (fn [a b] [a b]) (map #(s/index-of % \.) ((juxt first last) lines)) [0 (dec (count lines))])))

(defn moves-icy [grid [x y]]
  (case (grid [x y])
    nil [] \# []
    \. [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
    \^ [[x (dec y)]] \> [[(inc x) y]] \v [[x (inc y)]] \< [[(dec x) y]]))

(defn longest-icy
  ([in]
   (let [[start end] (start-end in)]
     (longest-icy (g/read-grid in) end (u/queue [{:loc start :len 0 :seen #{start}}]) 0)))
  ([grid end q max-len]
   (if (empty? q) max-len
       (let [{curr-loc :loc curr-len :len curr-seen :seen} (peek q)]
         (if (and (= curr-loc end) (> curr-len max-len)) (recur grid end (pop q) curr-len)
             (let [ms (remove curr-seen (moves-icy grid curr-loc))]
               (if (empty? ms) (recur grid end (pop q) max-len)
                   (let [next-moves (map (fn [m] {:loc m :len (inc curr-len) :seen (conj curr-seen curr-loc)}) ms)
                         next-q (apply conj (pop q) next-moves)]
                     (recur grid end next-q max-len)))))))))

(longest-icy "example23")              ;  ex 23-1: 94
(longest-icy "input23")                ; ans 23-1: 2086

(defn path? [grid [x y]]
  (let [t (grid [x y])]
    (and t (not= \# t))))

(defn moves-dry [grid [x y]]
  (case (grid [x y])
    (filter (fn [[xx yy]] (path? grid [xx yy])) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))

(defn deg [grid [x y]]
  (count (moves-dry grid [x y])))

(defn junctions [in]
  (let [grid (g/read-grid in)
        js (->> grid (filter (fn [[[x y] p]] (and (not= p \#) (> (deg grid [x y]) 2)))) keys)]
    (set (apply conj js (start-end in)))))

(defn jneighbors
  ([grid js j] (jneighbors grid (disj js j) j (u/queue [{:loc j :len 0 :seen #{j}}]) {}))
  ([grid js j q paths]
   (if (empty? q) paths
       (let [{curr-loc :loc curr-len :len curr-seen :seen} (peek q)]
         (if (contains? js curr-loc) (recur grid js j (pop q) (assoc paths (set [j curr-loc]) curr-len))
             (let [ms (remove curr-seen (moves-dry grid curr-loc))]
               (let [next-moves (map (fn [m] {:loc m :len (inc curr-len) :seen (conj curr-seen curr-loc)}) ms)
                     next-q (apply conj (pop q) next-moves)]
                 (recur grid js j next-q paths))))))))

(defn jpaths [in]
  (let [grid (g/read-grid in)
        js (junctions in)]
    (->> js (map (partial jneighbors grid js)) (apply merge))))

(defn jmoves [jpaths [x y]]
  (->> jpaths (filter (fn [[ns _]] (contains? ns [x y]))) (map (fn [[ns d]] {:dest (first (disj ns [x y])) :dist d}))))

(defn longest-dry
  ([in] (let [[start end] (start-end in) jpaths (jpaths in)]
          (longest-dry (g/read-grid in) end jpaths (u/queue [{:loc start :len 0 :seen #{start}}]) 0)))
  ([grid end jpaths q max-len]
   (if (empty? q) max-len
       (let [{curr-loc :loc curr-len :len curr-seen :seen} (peek q)]
         (if (and (= curr-loc end) (> curr-len max-len)) (recur grid end jpaths (pop q) curr-len)
           (let [ms (remove (fn [m] (contains? curr-seen (:dest m))) (jmoves jpaths curr-loc))]
             (if (empty? ms) (recur grid end jpaths (pop q) max-len)
                 (let [next-moves
                       (map (fn [m] {:loc (:dest m) :len (+ curr-len (:dist m)) :seen (conj curr-seen curr-loc)}) ms)
                       next-q (apply conj (pop q) next-moves)]
                   (recur grid end jpaths next-q max-len)))))))))

(longest-dry "example23")                ; ex  23-2: 154
(longest-dry "input23")                  ; ans 23-2: 6526
