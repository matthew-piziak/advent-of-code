(ns aoc.day15
  (:require [aoc.utils :as u]
            [clojure.core.matrix :as mat]
            [clojure.data.priority-map :as heap]))

(defn parse-matrix [in]
  (->> in
     u/read-lines
     (mapv seq)
     (mapv (partial mapv (comp #(Integer/parseInt %) (partial str))))
     mat/matrix))

(defn node [[x y] v]
  {:x x :y y :risk v :dist Integer/MAX_VALUE :prev-x nil :prev-y nil})

(defn parse-graph [m]
  (let [graph (mat/emap-indexed node m)]
    (assoc-in graph [0 0 :dist] 0)))

(def example (parse-graph (parse-matrix "example15")))

(defn unvisited [graph]
  (reduce (fn [m s] (conj m [[(s :x) (s :y)] (s :dist)])) (heap/priority-map) (mat/to-vector graph)))

(defn dijkstra [graph unvisited]
  (if (empty? unvisited) graph
      (let [[[ux uy] uv] (peek unvisited)
            uns (filter (pop unvisited) [[(inc ux) uy] [ux (inc uy)] [(dec ux) uy] [ux (dec uy)]])]
        (letfn [[update-neighbor [g un]
                 (let [n (get-in g un)
                       {x :x y :y risk :risk dist :dist} n
                       alt (+ uv risk)]
                   ;; need to also update unvisited
                   (if (< alt dist) {:x x :y y :alt alt :prev [x y]}))]
                [update-graph [g {x :x y :y alt :alt [ux uy] :prev}]
                 (-> g
                    (assoc-in [x y :dist] alt)
                    (assoc-in [x y :prev-x] ux)
                    (assoc-in [x y :prev-y] uy))]
                [update-unvisited [u {x :x y :y alt :alt [ux uy] :prev}]
                 (-> u (assoc [x y] alt))]]
          (let [updates (remove nil? (map (partial update-neighbor graph) uns))
                new-graph (reduce update-graph graph updates)
                new-unvisited (reduce update-unvisited (pop unvisited) updates)]
            (recur new-graph new-unvisited))))))

(defn part-one [in]
  (let [[l h] (mat/shape in)]
    ((((dijkstra in (unvisited in)) (dec l)) (dec h)) :dist)))

;;; ex15-1: 40
(part-one example)

(def input (parse-graph (parse-matrix "input15")))

;;; ans15-1: 498
(part-one input)

(def large (parse-graph (parse-matrix "large15")))

;;; 315
(part-one large)

(defn make-large [m]
  (apply mat/join-along 0
         (for [l (range 5)]
           (apply mat/join-along 1
                  (for [n (range 5)]
                    (mat/emap (fn [x] (let [s (+ n l x)] (if (> s 9) (- s 9) s))) m))))))

(defn part-two [in]
  (let [large (parse-graph (make-large (parse-matrix in)))
        [l h] (mat/shape large)]
    ((((dijkstra large (unvisited large)) (dec l)) (dec h)) :dist)))

;;; ex15-2: 315
(part-two "example15")

;;; ans15-2: 2901
(part-two "input15")
