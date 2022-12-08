(ns aoc.day07
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn ->fs
  ([input] (->fs {} [] input))
  ([fs path [l & lines]]
   (if-not l fs
     (let [[_ ls dir size file cd] (re-find #"[$] (ls)|dir (.+)|(\d+) (.+)|[$] cd (.+)" l)]
       (cond
         ls (recur fs path lines)
         dir (recur (assoc-in fs (conj path dir) {}) path lines)
         file (recur (assoc-in fs (conj path file) (Integer/parseInt size)) path lines)
         (= cd "/") (recur fs [] lines)
         (= cd "..") (recur fs (pop path) lines)
         :else (recur fs (conj path cd) lines))))))

(defn leaf-vals [fs]
  (->> fs fs->node-list (filter map?) (map #(reduce + 0 (filter int? (fs->node-list %))))))

(defn fs->size [fs]
  (->> fs leaf-vals (filter #(<= % 100000)) (reduce + 0)))

;;; ex 07-1: 95437
(->> "example07" u/read-lines ->fs fs->size)

;;; ans 07-1: 1390824
(->> "input07" u/read-lines ->fs fs->size)

(defn fs->node-list [fs]
  (tree-seq map? vals fs))

(defn space-needed [fs]
  (->> fs leaf-vals first (- 70000000) (- 30000000)))

(defn deletee-size [fs]
  (->> fs leaf-vals (filter #(>= % (space-needed fs))) (apply min)))

;;; ex 07-2: 24933642
(->> "example07" u/read-lines ->fs deletee-size)

;; ans 07-2: 7490863
(->> "input07" u/read-lines ->fs deletee-size)
