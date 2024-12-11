(ns aoc.day09
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn read-disk-map [in]
  (->> in u/read-resource s/trim-newline (re-seq #"\d") (map #(Integer/parseUnsignedInt %))))

(defn inflate
  ([in] (inflate (read-disk-map in) [] 0))
  ([disk-map inflated id]
   (if (= 1 (count disk-map)) (apply conj inflated (repeat (first disk-map) id))
       (let [[file spaces & remainder] disk-map]
         (recur remainder (apply conj (apply conj inflated (repeat file id)) (repeat spaces :s)) (inc id))))))

(defn defrag [inflated defragged]
  (if (empty? inflated) defragged
      (if (= :s (first inflated))
        (if (= :s (last inflated)) (recur (drop-last inflated) defragged)
            (recur (drop-last (rest inflated)) (conj defragged (last inflated))))
          (recur (rest inflated) (conj defragged (first inflated))))))

(defn chksum [defragged]
  (map-indexed * defragged))

(apply + (chksum (defrag (inflate "example09") []))) ; ex 09-1: 1928
(apply + (chksum (defrag (inflate "input09") [])))   ; an 09-1: 6216544403458

(defn inflate-blocks
  ([in] (inflate-blocks (read-disk-map in) [] 0 0))
  ([disk-map inflated id loc]
   (if (= 1 (count disk-map)) (apply conj inflated [{:id id :len (first disk-map) :loc loc}])
       (let [[file spaces & remainder] disk-map]
         (recur remainder (apply conj inflated [{:id id :len file :loc loc}])
                (inc id) (+ loc file spaces))))))

(defn inflate-spaces
  ([in] (inflate-spaces (read-disk-map in) [] 0))
  ([disk-map inflated loc]
   (if (= 1 (count disk-map)) inflated
       (let [[file spaces & remainder] disk-map]
         (if (zero? spaces) (recur remainder inflated (+ loc file spaces))
           (recur remainder (conj inflated {:slen spaces :loc (+ loc file)}) (+ loc file spaces)))))))

(defn defrag-block [spaces block]
  (if (empty? spaces) :unmoved
      (let [{loc :loc id :id len :len} block
            {sloc :loc slen :slen} (first spaces)]
        (cond
          (>= sloc loc) :unmoved
          (>= slen len) {:loc sloc :id id :len len}
          :else (recur (rest spaces) block)))))

(defn update-spaces [spaces moved-block updated]
  (if (empty? spaces) (reverse updated)
      (let [{loc :loc _ :id len :len} moved-block
            {sloc :loc slen :slen} (first spaces)]
        (if (= sloc loc)
          (if (= slen len) (recur (rest spaces) moved-block updated)
              (recur (rest spaces) moved-block (conj updated {:loc (+ sloc len) :slen (- slen len)})))
          (recur (rest spaces) moved-block (conj updated {:loc sloc :slen slen}))))))

(defn defrag-blocks [blocks spaces moved]
  (if (empty? blocks) moved
      (let [db (defrag-block spaces (first blocks))]
        (if (= db :unmoved) (recur (rest blocks) spaces (conj moved (first blocks)))
            (recur (rest blocks) (update-spaces spaces db '()) (conj moved db))))))

(defn chksum-block [block]
  (apply + (map (fn [i] (* (:id block) i)) (range (:loc block) (+ (:loc block) (:len block))))))

(defn chksum-defragged [in]
  (apply + (map chksum-block (defrag-blocks (reverse (inflate-blocks in)) (inflate-spaces in) []))))

(chksum-defragged "example09")          ; ex 09-1: 2858
(chksum-defragged "input09")            ; ex 09-1: 6237075041489
