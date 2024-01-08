(ns aoc.day18
  (:require [aoc.utils :as u]
            [instaparse.core :as p]
            [clojure.core.matrix :as mat]))

(mat/set-current-implementation :vectorz)

(defn read-dig [line]
  (let [parse (p/parser "dig = dir <ws> len <ws> color
                         dir = 'R' | 'L' | 'D' | 'U'
                         len = #'[0-9]+'
                         color = <'(#'> #'[0-9a-f]+' <')'>
                         ws = #' '*")
        transform {:len (fn [n] [:len (Long/parseLong n)])
                   :dir (fn [d] (case d "R" [1 0] "L" [-1 0] "D" [0 1] "U" [0 -1]))
                   :dig (fn [dir [_ len] [_ color]] {:len len :dir dir :color color})}]
    (->> line parse (p/transform transform))))

(defn dig->v [{:keys [len dir]}]
  (mat/matrix (mapv #(*' len %) dir)))

(defn digs->coords [digs]
  (letfn [(dig->v [{:keys [len dir]}] (mat/matrix (mapv #(*' len %) dir)))]
    (->> digs (map dig->v) (reductions mat/add [0 0]))))

(defn digs->interior [digs]
  (->> digs digs->coords (partition 2 1) (transduce (map (comp #(/ % 2) mat/det mat/matrix)) +' 0)))

(defn digs->perimeter [digs]
  (transduce (map :len) +' 0 digs))

(defn digs->area [digs]
  (let [i (digs->interior digs)
        p (digs->perimeter digs)]
    (long (inc (+' i (/ p 2))))))

;;; ex 18-1: 62
(digs->area (map read-dig (u/read-lines "example18")))

;;; ans 18-1: 36725
(digs->area (map read-dig (u/read-lines "input18")))

(defn color->dig [hex]
  {:len (Long/parseLong (apply str (butlast hex)) 16) :dir(case (last hex) \0 [1 0] \2 [-1 0] \1 [0 1] \3 [0 -1])})

;;; ex 18-2: 952408144115
(digs->area (map (comp color->dig :color read-dig) (u/read-lines "example18")))

;;; ans 18-2: 97874103749720
(digs->area (map (comp color->dig :color read-dig) (u/read-lines "input18")))
