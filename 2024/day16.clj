(ns aoc.day16
  (:require
   [aoc.utils :as u]
   [aoc.grid :as g]
   [clojure.set :as set]
   [astar.core :as astar]
   [dijkstra.core :as dijkstra]))

(defn endpoints [grid]
  (let [inverted (set/map-invert grid)]
    [(inverted \S) (inverted \E)]))

(defn turns [node]
  (let [turns (case (:dir node) :n [:e :w] :s [:e :w] :e [:n :s] :w [:n :s])]
    (map (fn [d] [(assoc node :dir d) 1000]) turns)))

(defn forward-loc [[x y] dir]
  (case dir :n [x (dec y)] :s [x (inc y)] :e [(inc x) y] :w [(dec x) y]))

(defn forward [grid node]
  (let [forward-loc (forward-loc (:loc node) (:dir node))]
    (if (= \# (grid forward-loc)) [] [[(-> node (assoc :loc forward-loc)) 1]])))

(defn goal [end-loc node]
  (if (= end-loc (:loc node)) [[(assoc node :dir :g) 0]] []))

(defn neighbors [grid node end-loc]
  (if (= :g (:dir node)) []
      (into {} (concat (turns node) (forward grid node) (goal end-loc node)))))

(defn score [route]
  (let [dirs (butlast (map :dir route))
        turns (->> (conj dirs :e) (partition-by identity) count dec)]
    (+ (* turns 1000) (- (count dirs) turns))))

(defn heuristic [end-loc node]
  (let [[ex ey] end-loc
        [nx ny] (:loc node)
        requires-turn (or (not= ex nx) (not= ey ny))]
    (+ (g/mh-dist [ex ey] [nx ny]) (if requires-turn 1000 0))))

(defn best-cost [in]
  (let [grid (g/read-grid in)
        [start-loc end-loc] (endpoints grid)
        graph (fn [node] (map first (neighbors grid node end-loc)))
        dist (fn [from to] (get (neighbors grid from end-loc) to))
        h (partial heuristic end-loc)
        route (astar/route graph dist h {:loc start-loc :dir :e} {:loc end-loc :dir :g})]
    (score route)))

(best-cost "example16")                 ; ex 16-1: 7036
(best-cost "large16")                   ; lg 16-1: 11048
(best-cost "input16")                   ; an 16-1: 98416

(defn ->vertices [[k v]]
  (if (= v \#) []
      (map (fn [d] {:loc k :dir d}) [:n :s :e :w])))

(defn ->edges [grid end-loc node]
  (into {} (map (fn [[k v]] {[node k] v}) (neighbors grid node end-loc))))

(defn optimal-nodes [costs dist inverted-edges nodes optimal]
  (println "Queue Size: " (count nodes))
  (println "Optimal Size: " (count optimal))
  (if (empty? nodes) optimal
      (let [node (peek nodes)
            [cost _] (costs node)
            predecessors (map (juxt identity costs #(dist % node)) (inverted-edges node))
            new-optimal (set (map first (filter (fn [[_ [ c _] d]] (= (+ c d) cost)) predecessors)))]
        (recur costs
               dist
               inverted-edges
               (apply conj (pop nodes) (set/difference new-optimal optimal))
               (set/union optimal new-optimal)))))

(defn best-seats [in]
  (let [grid (g/read-grid in)
        [start-loc end-loc] (endpoints grid)
        dist (fn [from to] (get (neighbors grid from end-loc) to))
        goal {:loc end-loc :dir :g}
        start {:loc start-loc :dir :e}
        vertices (into #{} (conj (mapcat ->vertices grid) goal))
        edges (into {} (mapcat (partial ->edges grid end-loc) vertices))
        inverted-edges (apply merge-with (fn [s1 s2] (apply merge s1 s2)) (map (fn [[s d]] {d #{s}}) (keys edges)))
        costs (dijkstra/dijkstra vertices edges start)
        [_ pregoal] (costs goal)]
    (count (set (map :loc (optimal-nodes costs dist inverted-edges (u/queue [pregoal]) #{pregoal}))))))

(best-seats "example16")                ; ex 16-1: 45
(best-seats "large16")                  ; lg 16-1: 64
(best-seats "input16")                  ; an 16-1: 471
