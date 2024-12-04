(ns aoc.day04
    (:require [aoc.utils :as u]
              [clojure.core.matrix :as mat]))

(defn xmass [in]
  (let [ls (u/read-lines in)
        n (count ls)
        tls (map u/chars->str (u/transpose ls))
        r (range (- n) (inc n))
        dls (map u/chars->str (map (partial mat/diagonal (mat/matrix (map vec ls))) r))
        tdls (let [m (->> (map vec ls) u/transpose (mapv reverse) mat/matrix)]
               (map u/chars->str (map (partial mat/diagonal m) r)))
        all-ls (concat ls tls dls tdls)]
    (transduce (map (comp count (partial re-seq #"(?=(XMAS|SAMX))"))) + 0 all-ls)))

(xmass "example04")                     ; ex 04-1: 18
(xmass "input04")                       ; an 04-1: 2646

(defn m->sms [m]
  (for [i (range 0 (- (count m) 2))
        j (range 0 (- (count m) 2))]
    (mat/matrix (mat/submatrix m [[i 3] [j 3]]))))

(defn xmas? [sm]
  (let [center (get-in sm [1 1])
        corners (map #(get-in sm %) [[0 0] [0 2] [2 0] [2 2]])]
    (and (= center \A) (= (frequencies corners) {\M 2 \S 2}) (not= (first corners) (last corners)))))

(defn masxs [in]
  (->> in u/read-lines (map vec) mat/matrix m->sms (filter xmas?) count))

(masxs "example04")                     ; ex 04-2: 9
(masxs "input04")                       ; an 04-2: 2000
