(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.core.match :as match]
            [clojure.string :as s]))

(let [[register-strings [program-string]] (u/read-paras "example17")
      program (map #(Integer/parseInt %) (re-seq #"\d+" program-string))
      registers (map #(Integer/parseInt (re-find #"\d+" %)) register-strings)]
  [program registers])

(defn opcode->inst [opcode]
  (case opcode
    :done :done
    0 :adv
    1 :bxl
    2 :bst
    3 :jnz
    4 :bxc
    5 :out
    6 :bdv
    7 :cdv))

(defn combo [registers operand]
  (case operand
    0 0
    1 1
    2 2
    3 3
    4 (registers :a)
    5 (registers :b)
    6 (registers :c)
    7 :reserved))

(defn adv [registers operand]
  (assoc registers :a (bit-shift-right (registers :a) (combo registers operand))))

(defn bxl [registers operand]
  (update registers :b #(bit-xor % operand)))

(defn bst [registers operand]
  (assoc registers :b (mod (combo registers operand) 8)))

(defn jnz [registers operand]
  (if (zero? (registers :a))
    :no-op
    [:jump operand]))

(defn bxc [registers _]
  (assoc registers :b (bit-xor (registers :b) (registers :c))))

(defn out [registers operand]
  (mod (combo registers operand) 8))

(defn bdv [registers operand]
  (assoc registers :b (bit-shift-right (registers :a) (combo registers operand))))

(defn cdv [registers operand]
  (assoc registers :c (bit-shift-right (registers :a) (combo registers operand))))

(defn run
  ([program registers] (run program registers 0 []))
  ([program registers inst-pointer output]
   (let [inst (opcode->inst (nth program inst-pointer :done))
         operand (nth program (inc inst-pointer) :done)]
     (case inst
       :done [registers output]
       :adv (recur program (adv registers operand) (+ 2 inst-pointer) output)
       :bxl (recur program (bxl registers operand) (+ 2 inst-pointer) output)
       :bst (recur program (bst registers operand) (+ 2 inst-pointer) output)
       :jnz
       (match/match (jnz registers operand)
                    [:jump dest-pointer] (recur program registers dest-pointer output)
                    :else (recur program registers (+ 2 inst-pointer) output))
       :bxc (recur program (bxc registers operand) (+ 2 inst-pointer) output)
       :out (recur program registers (+ 2 inst-pointer) (conj output (out registers operand)))
       :bdv (recur program (bdv registers operand) (+ 2 inst-pointer) output)
       :cdv (recur program (cdv registers operand) (+ 2 inst-pointer) output)))))

(run [2 6] {:c 9})
(run [5 0 5 1 5 4] {:a 10})
(run [0 1 5 4 3 0] {:a 2024})
(run [1 7] {:b 29})
(run [4 0] {:b 2024 :c 43690})

; ex 17-1: 4,6,3,5,6,3,5,2,1,0
(s/join "," (second (run [0 1 5 4 3 0] {:a 729 :b 0 :c 0})))
; an 17-1: 3,4,3,1,7,6,5,6,0
(s/join "," (second (run [2 4 1 5 7 5 4 5 0 3 1 6 5 5 3 0] {:a 63281501})))

(defn prune
  ([program xor1 xor2] (prune program xor1 xor2 #{0}))
  ([program xor1 xor2 a-set]
   (if (empty? program) a-set
     (let [out (last program)
           new-set (->> a-set
                        (mapcat (fn [a’]
                                  (for [c (range 8)
                                        :let [a (bit-or (bit-shift-left a’ 3) c)
                                              b2 (bit-xor c xor1)
                                              cval (bit-shift-right a b2)
                                              produced (bit-and 7 (bit-xor (bit-xor b2 cval) xor2))]
                                        :when (= produced out)] a))) set)]
       (recur (butlast program) xor1 xor2 new-set)))))

(defn min-initial-a
  [program]
  (let [xor1 (nth program 3)
        xor2 (nth program 11)]
    (->> (prune program xor1 xor2)
         (remove zero?)
         (apply min))))

; ex 17-2: 117440
; an 17-2: 109019930331546
(min-initial-a [2 4 1 5 7 5 4 5 0 3 1 6 5 5 3 0])
