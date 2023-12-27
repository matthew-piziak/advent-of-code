(ns aoc.day12
  (:require [aoc.utils :as u]
            [instaparse.core :as p]))

(defn read-record [line]
  (let [parse (p/parser "record = conditions <ws> summary
                       conditions = condition+
                       condition = o | d | u
                       o = <'.'>
                       d = <'#'>
                       u = <'?'>
                       summary = num (<','> num)*
                       num = #'[0-9]+'
                       ws = #' '*")
        transform {:num (fn [n] [:num (Long/parseLong n)])
                   :summary (fn [& nums] {:summary (map second nums)})
                   :conditions (fn [& cs] {:conditions (flatten (map second cs))})
                   :record (fn [& attrs] (apply merge attrs))}]
    (->> line parse (p/transform transform))))

(def ways
  (memoize
   (fn [{:keys [conditions summary]}]
     (letfn [(with-summary [cs] (ways {:conditions (drop-while #{:o} cs) :summary summary}))]
       (cond
         (empty? conditions) (cond (empty? summary) 1 :else 0)
         (empty? summary) (if (every? #{:o :u} conditions) 1 0)
         (< (count (filter #{:d :u} conditions)) (apply + summary)) 0
         :else (let [[c & cs] conditions [s & ss] summary]
                 (case c
                   :d (if-not (every? #{:d :u} (take s conditions)) 0
                        (let [r (drop s conditions)]
                          (if (empty? r) (ways {:conditions '() :summary ss})
                              (case (first r)
                                :d 0
                                :o (ways {:conditions (drop s conditions) :summary ss})
                                :u (ways {:conditions (drop (inc s) conditions) :summary ss})))))
                   :o (with-summary cs)
                   :u (+ (with-summary (conj cs :d)) (with-summary cs)))))))))

;; ex 12-1: 21
(u/sum-lines "example12" (map (comp ways read-record)))

;;; ans 12-1: 7718
(u/sum-lines "input12" (map (comp ways read-record)))

(defn unfold [{:keys [conditions summary]}]
  {:conditions (flatten (interpose :u (repeat 5 conditions))) :summary (flatten (repeat 5 summary))})

;;; ex 12-2: 525152
(u/sum-lines "example12" (map (comp ways unfold read-record)))

;;; ans 12-2: 128741994134728
(u/sum-lines "input12" (map (comp ways unfold read-record)))
