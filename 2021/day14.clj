(ns aoc.day14
    (:require [aoc.utils :as u]
              [clojure.string :as s]))

(def example (u/read-paras "example14"))

(defn template [in] (ffirst in))

(defn rules [in] (into {} (map #(s/split % #" -> ") (second in))))

(defn insert [rules pair]
  (apply str ((juxt first rules second) pair)))

(defn step [rules template]
  (let [monomers (map (comp (partial insert rules) (partial apply str)) (partition 2 1 template))]
    (apply str (concat (first monomers) (map (comp (partial apply str) rest) (rest monomers))))))

(defn polymerize [rules template generations]
  (last (take (inc generations) (iterate (partial step rules) template))))

(defn quantities [in generations]
  (frequencies (seq (polymerize (rules in) (template in) generations))))

(defn part-one [in]
  (let [qs (vals (quantities in 10))]
    (- (apply max qs) (apply min qs))))

(def input (u/read-paras "input14"))

;;; ex14-1: 1588
(part-one example)

;;; ans14-1: 2584
(part-one input)

;;; That was too slow! Let's cache some bigger chunks and their resulting tranformations.
(defn make-cache [rules]
  (let [vs (into #{} (vals rules))
        quartets (for [a vs b vs c vs d vs] (let [q (str a b c d)] {(str a b c d) (step rules q)}))
        quintets (for [a vs b vs c vs d vs e vs] (let [q (str a b c d e)] {(str a b c d e) (step rules q)}))]
    (apply merge (concat quartets quintets))))

(def example-rules (make-cache (rules example)))

(def input-rules (make-cache (rules input)))

;;; The problem can be divided into overlapping chunks. Count the chunks and subtract the overlaps.
(defn template->chunks [template]
  (let [ps (map (partial apply str) (partition-all 5 4 template))]
    {:chunks (frequencies ps) :overlaps (frequencies (map first (rest ps)))}))

;;; We have some cycles we can take advantage of. 9s chunk into 5s, which step into 9s. Same for 7 and 4.
(defn split-chunk [chunk]
  (case (count chunk)
    9 (mapv (partial apply str) (partition 5 4 chunk))
    7 (mapv (partial apply str) (partition 4 3 chunk))))
(defn overlap [chunk]
  (case (count chunk)
    9 (nth chunk 4)
    7 (nth chunk 3)))

;;; Much faster. Now we're counting chunks instead of manipulating one large string.
(defn chunks-step [rules cs]
  (let [{chunks :chunks overlaps :overlaps} cs]
    (update-in
     (apply
      (partial merge-with (partial merge-with +))
      (for [[c m] chunks]
        (let [stepped-c (rules c)]
          (if (<= (count stepped-c) 5) {:chunks {stepped-c m}}
              {:chunks (into {} (map (fn [[k v]] [k (* v m)]) (frequencies (split-chunk stepped-c))))
               :overlaps {(overlap stepped-c) m}}))))
     [:overlaps] (partial merge-with + (cs :overlaps)))))

(defn chunks->quantities [cs]
  (let [{chunks :chunks overlaps :overlaps} cs
        chunk-qs (apply
                  (partial merge-with +)
                  (for [[q m] chunks] (into {} (for [[k v] (frequencies q)] [k (* v m)]))))]
    (merge-with - chunk-qs overlaps)))

(defn quantities->score [qs]
  (let [vs (vals qs)]
    (- (apply max vs) (apply min vs))))

(defn chunks-step-n [rules cs gens]
  (if (zero? gens) (-> cs chunks->quantities quantities->score)
      (recur rules (chunks-step rules cs) (dec gens))))

;;; ex14-1: 2188189693529
(chunks-step-n example-rules (template->chunks (template example)) 40)

;;; ans14-1: 3816397135460
(chunks-step-n input-rules (template->chunks (template input)) 40)

;;; execution time on full input: 200ms
