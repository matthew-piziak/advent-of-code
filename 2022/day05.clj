(ns aoc.day05
  (:require [aoc.utils :as u]))

(def example-stacks ['(\N \Z) '(\D \C \M) '(\P)])

(def example-moves [{:n 1 :from 2 :to 1} {:n 3 :from 1 :to 3} {:n 2 :from 2 :to 1} {:n 1 :from 1 :to 2}])

(defn mover [transferf stacks {n :n from :from to :to}]
  (let [new-from-stack (drop n (nth stacks (dec from)))
        new-to-stack (concat (transferf (take n (nth stacks (dec from)))) (nth stacks (dec to)))
        new-stacks (assoc stacks (dec from) new-from-stack (dec to) new-to-stack)]
    new-stacks))

(def crane (partial mover reverse))

(defn stack-tops [stacks moves movef]
  (apply str (map first (reduce movef stacks moves))))

(def input-stacks ['(\T \R \G \W \Q \M \F \P) '(\R \F \H) '(\D \S \H \G \V \R \Z \P)
                   '(\G \W \F \B \P \H \Q) '(\H \J \M \S \P) '(\L \P \R \S \H \T \Z \M)
                   '(\L \M \N \H \T \P) '(\R \Q \D \F) '(\H \P \L \N \C \S \D)])

(def parse-move
  (comp (fn [[n from to]] {:n n :from from :to to}) (partial map #(Integer/parseInt %)) #(re-seq #"\d+" %)))

(def input-moves
  (->> "input05" u/read-lines (map parse-move)))

;;; ex 05-1: CMZ
(stack-tops example-stacks example-moves crane)

;;; ans 05-1: TPGVQPFDH
(stack-tops input-stacks input-moves crane)

(def crate-mover-9001 (partial mover reverse))

;;; ex 05-2: MCD
(stack-tops example-stacks example-moves move9001)

;;; ans 05-2: DMRDFRHHH
(stack-tops input-stacks input-moves move9001)
