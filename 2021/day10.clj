(ns aoc.day10
  (:require [aoc.utils :as u]
            [instaparse.core :as p]
            [clojure.string :as s]
            [fastmath.stats :as stats]))

(def parse
  (p/parser
   "expr = (round | square | curly | angle)*
    round = '(' expr ')'
    square = '[' expr ']'
    curly = '{' expr '}'
    angle = '<' expr '>'"))

(def success? (complement p/failure?))
(assert (success? (parse "()")))
(assert (success? (parse "[]")))
(assert (success? (parse "([])")))
(assert (success? (parse "{()()()}")))
(assert (success? (parse "<([{}])>")))
(assert (success? (parse "[<>({}){}[([])<>]]")))
(assert (success? (parse "(((((((((())))))))))")))
(assert (p/failure? (parse "(]")))
(assert (p/failure? (parse "{()()()>")))
(assert (p/failure? (parse "(((()))}")))
(assert (p/failure? (parse "<([]){()}[{}])")))

(def example (u/read-lines "example10"))

(defn diagnose [chunk]
  (let [parsed (parse chunk)]
    (if (p/failure? parsed)
      (let [{column :column text :text} parsed]
        (if (> column (count text))
          :incomplete
          {:corrupt (get text (dec column))}))
      :correct)))

(defn part-one [in]
  (->> in
     (map diagnose)
     (map :corrupt)
     (remove nil?)
     frequencies
     (map (fn [[err n]] (* n ({\) 3 \] 57 \} 1197 \> 25137} err))))
     (apply +)))

;; ex10-1: 26397
(part-one example)

(def input (u/read-lines "input10"))

;; ans10-1: 323691
(part-one input)

(defn complete
  ([line] (complete line ""))
  ([line completion]
   (let [res (parse (str line completion))]
     (if (success? res) completion
         (recur line (str completion (:expecting (first (:reason res)))))))))

(defn score-completion
  ([completion] (score-completion completion 0))
  ([completion score]
   (if (empty? completion) score
     (recur (rest completion) (+ (case (first completion) \) 1 \] 2 \} 3 \> 4) (* 5 score))))))

(defn part-two [in]
  (->> in
     (filter (fn [l] (= :incomplete (diagnose l))))
     (map (comp score-completion complete))
     stats/median
     long))

;;; ex10-2: 288957
(part-two example)

;;; ans10-2: 2858785164
(part-two input)
