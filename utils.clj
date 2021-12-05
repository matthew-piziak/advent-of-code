(ns aoc.utils
    (:require
     [clojure.java.io :as io]
     [clojure.string :as s]))

(defn read-resource [i]
  (slurp (io/resource i)))

(defn read-lines [i]
  (line-seq (io/reader (io/resource i))))

(defn split-by
  ([pred]
   (comp (drop-while pred)
         (partition-by pred)
         (take-nth 2)))
  ([pred coll]
   (sequence (split-by pred) coll)))

(defn read-paras [i]
  (split-by empty? (read-lines i)))

(defn read-ints [i]
  (map #(Integer/parseInt %) (read-lines i)))

(defn read-longs [i]
  (map #(Long/parseLong %) (read-lines i)))

(defn seq-peek
  "Calls callback after n entries in s are evaluated."
  [n callback s]
  (let [call-and-return
       (fn [iter data]
         (if (zero? (rem iter n)) (callback iter data) nil)
         data)]
    (map call-and-return
         (iterate inc 1) s)))

(defn unlines [ss]
  (s/join "\n" ss))

(defn uncommas [s]
  (s/split s #","))
