(ns aoc.day11)

(defn div? [div num]
  (zero? (mod num div)))

(def example-mod 96577)
(def example
  {0 {:items [79 98] :op #(mod (* 19 %) example-mod) :test #(div? 23 %) :true 2 :false 3 :inspected 0}
   1 {:items [54 65 75 74] :op #(mod (+ 6 %) example-mod) :test #(div? 19 %) :true 2 :false 0 :inspected 0}
   2 {:items [79 60 97] :op #(mod (Math/pow % 2) example-mod) :test #(div? 13 %) :true 1 :false 3 :inspected 0}
   3 {:items [74] :op #(mod (+ 3 %) example-mod) :test #(div? 17 %) :true 0 :false 1 :inspected 0}})

(def input-mod (* 7 19 5 11 17 13 2 3))
(def input
  {0 {:items [57 58] :op #(mod (* 19 %) input-mod) :test #(div? 7 %) :true 2 :false 3 :inspected 0}
   1 {:items [66 52 59 79 94 73] :op #(mod (+ 1 %) input-mod) :test #(div? 19 %) :true 4 :false 6 :inspected 0}
   2 {:items [80] :op #(mod (+ 6 %) input-mod) :test #(div? 5 %) :true 7 :false 5 :inspected 0}
   3 {:items [82 81 68 66 71 83 75 97] :op #(mod (+ 5 %) input-mod) :test #(div? 11 %) :true 5 :false 2 :inspected 0}
   4 {:items [55 52 67 70 69 94 90] :op #(mod (* % %) input-mod) :test #(div? 17 %) :true 0 :false 3 :inspected 0}
   5 {:items [69 85 89 91] :op #(mod (+ 7 %) input-mod) :test #(div? 13 %) :true 1 :false 7 :inspected 0}
   6 {:items [75 53 73 52 75] :op #(mod (* 7 %) input-mod) :test #(div? 2 %) :true 0 :false 4 :inspected 0}
   7 {:items [94 60 79] :op #(mod (+ 2 %) input-mod) :test #(div? 3 %) :true 1 :false 6 :inspected 0}})

(defn toss [worryf op test tm fm item]
  (let [new-item (op item)
        new-worry (worryf new-item)]
    (if (test new-worry) [tm new-worry] [fm new-worry])))

(defn turn [worryf {items :items op :op test :test tm :true fm :false}]
  (map (partial toss worryf op test tm fm) items))

(defn round-idx [worryf monkeys midx]
  (let [inspections (turn worryf (monkeys midx))]
    (-> (reduce (fn [ms [m i]] (update-in ms [m :items] #(conj % i))) monkeys inspections)
     (assoc-in [midx :items] [])
     (update-in [midx :inspected] (partial + (count inspections))))))

(defn round [worryf monkeys]
  (reduce (fn [ms midx] (round-idx worryf ms midx)) monkeys (keys monkeys)))

(defn monkey-business [in worryf rounds]
  (let [monkeys (nth (iterate (partial round worryf) in) rounds)]
    (->> monkeys vals (map :inspected) sort (take-last 2) (apply *))))

;;; ex 11-1: 10605
(monkey-business example #(int (Math/floor (/ % 3))) 20)

;;; ans 11-1: 50830
(monkey-business input #(int (Math/floor (/ % 3))) 20)

;;; ex 11-2: 2713310158
(monkey-business example identity 10000)

;;; ans 11-2: 14399640002
(monkey-business input identity 10000)
