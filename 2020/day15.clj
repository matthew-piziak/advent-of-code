(ns aoc.day15)

(def example = [0 3 6])

(def input = [9 3 1 0 8 4])

(defn nxt [[seen n idx]]
  (let [next-n (if (contains? seen n) (- idx (seen n)) 0)
        next-seen (assoc seen n idx)
        next-idx (inc idx)]
    [next-seen next-n next-idx]))

;;; ex 15-1: 436
(last (take (- 2020 2) (iterate nxt [{0 1, 3 2} 6 3])))

;;; ans 15-1: 371
(last (take (- 2020 5) (iterate nxt [{9 1, 3 2, 1 3, 0 4, 8 5} 4 6])))

;;; ex 30000000: 175594
(time (nth (last (take (- 30000000 2) (iterate nxt [{0 1, 3 2} 6 3]))) 1))

;;; ans 15-2: 352
(time (last (take (- 30000000 5) (iterate nxt [{9 1, 3 2, 1 3, 0 4, 8 5} 4 6]))))
