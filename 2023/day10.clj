(ns aoc.day10
  (:require [aoc.utils :as u]
            [clojure.set :as set]))

(defn read-grid [input]
  (apply merge (map-indexed (fn [y l] (apply merge (map-indexed (fn [x c] {[x y] c}) (seq l)))) (u/read-lines input))))

(defn start-loc [grid] (ffirst (filter (fn [[k v]] (= v \S)) grid)))
(defn adj-locs [[x y]] {:up [x (dec y)] :rt [(inc x) y] :dn [x (inc y)] :lt [(dec x) y]})
(defn adjs [grid adj-locs] (for [[k v] adj-locs :when (grid v)] [k (grid v) v]))

(defn connection? [k [d c _]]
  (let [cs {:up #{\│ \┐ \┌} :rt #{\─ \┐ \┘} :dn #{\└ \│ \┘} :lt #{\└ \─ \┌}}
        disds (fn [ds] (if (empty? ds) cs (apply (partial assoc cs) (mapcat (fn [d] [d #{}]) ds))))]
    (((disds (case k \S [] \│ [:rt :lt] \─ [:up :dn] \┐ [:up :rt] \┌ [:up :lt] \┘ [:dn :rt] \└ [:dn :lt])) d) c)))

(defn first-steps [grid]
  (into [] (filter #(connection? \S %) (adjs grid (adj-locs (start-loc grid))))))

(defn step [grid [d c l]]
  (letfn [(not-opposite? [[e _ _]] (not= e (case d :up :dn :rt :lt :dn :up :lt :rt)))]
    (first (filter (partial not-opposite?) (filter (partial connection? c) (adjs grid (adj-locs l)))))))

(defn find-loop
  ([input] (let [grid (read-grid input)] (find-loop grid (first-steps grid) 1 #{(start-loc grid)})))
  ([grid [[cwd cwc cwl] [wsd wsc wsl]] steps loop-locs]
   (if (= [cwc cwl] [wsc wsl]) [steps (conj loop-locs cwl)]
       (recur grid [(step grid [cwd cwc cwl]) (step grid [wsd wsc wsl])] (inc steps) (conj loop-locs cwl wsl)))))

(defn farpoint [input] (-> input find-loop first))
(defn loop-locs [input] (-> input find-loop second))

;;; exs 10-1: 4, 8
(farpoint "example10")
(farpoint "medium10")

;;; ans 10-1: 6820
(farpoint "input10")

(defn count-interior [input input-s-replaced]
  (let [grid (read-grid input-s-replaced)
        loop-locs (loop-locs input)
        w (apply max (map first (keys grid)))]
    (loop [x 0 y 0 interior #{} interior? false bend nil]
      (let [c (grid [x y])]
        (cond
          (> x w) (recur 0 (inc y) interior false nil)
          (> y w) (count (set/difference interior loop-locs))
          (loop-locs [x y]) (cond
                              (= c \│) (recur (inc x) y interior (not interior?) nil)
                              bend (case [bend c]
                                     [\└ \┐] (recur (inc x) y interior (not interior?) nil)
                                     [\┌ \┘] (recur (inc x) y interior (not interior?) nil)
                                     [\└ \─] (recur (inc x) y interior interior? \└)
                                     [\┌ \─] (recur (inc x) y interior interior? \┌)
                                     [\└ \┘] (recur (inc x) y interior interior? nil)
                                     [\┌ \┐] (recur (inc x) y interior interior? nil))
                              (= c \└) (recur (inc x) y interior interior? \└)
                              (= c \┌) (recur (inc x) y interior interior? \┌))
          :else (recur (inc x) y (if interior? (conj interior [x y]) interior) interior? nil))))))

;;; exs 10-2: 1, 10
(count-interior "example10" "examplerepl10")
(count-interior "extra10" "extrarepl10")

;;; ans 10-2: 337
(count-interior "input10" "inputrepl10")
