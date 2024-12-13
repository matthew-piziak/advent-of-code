(ns aoc.day13
  (:require
   [aoc.utils :as u]
   [instaparse.core :as p]
   [clojure.core.matrix :as mat]
   [clojure.core.matrix.linear :as lin]))

(mat/set-current-implementation :vectorz)

(defn solve [machines prize]
  (let [solution (lin/solve machines prize)
        rounded-solution (mapv #(Math/round ^double %) solution)
        check (mat/mmul machines rounded-solution)]
    (if (= check prize) rounded-solution [0 0])))

(defn read-machine [prize-offset para]
  (let [parse (p/parser "machine = a <nl> b <nl> prize
                         a = <'Button A:'> <ws> coords
                         b = <'Button B:'> <ws> coords
                         coords = <'X+'> num <', '> <'Y+'> num
                         prize = <'Prize:'> <ws> <'X='> num <', '> <'Y='> num
                         num = #'[0-9]+'
                         ws = #'\\s*'
                         nl = #'\\n'+")
        transform {:num #(Integer/parseInt %)
                   :coords (fn [x y] [x y])
                   :prize (fn [x y] (mat/matrix [(+ x prize-offset) (+ y prize-offset)]))
                   :machine (fn [[_ [xa ya]] [_ [xb yb]] p] (solve (mat/matrix [[xa xb] [ya yb]]) p))}]
    (->> para u/unlines parse (p/transform transform))))

(defn cost [in prize-offset]
  (transduce (map (comp (fn [[a b]] (+ (* 3 a) b)) (partial read-machine prize-offset))) + 0 (u/read-paras in)))

(cost "example13" 0)                    ; ex 13-1: 480
(cost "input13" 0)                      ; an 13-1: 31552
(cost "example13" 10000000000000)       ; ex 13-2: 875318608908
(cost "input13" 10000000000000)         ; an 13-2: 95273925552482
