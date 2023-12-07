(ns aoc.day06)

(defn ws [{t :t r :r}]
  (loop [ms 0 ws 0]
    (if (= ms t) ws
        (recur (inc ms) (if (> (* ms (- t ms)) r) (inc ws) ws)))))

;;; ex 06-1: 288
(apply * (map ws [{:t 7 :r 9} {:t 15 :r 40} {:t 30 :r 200}]))

;;; ans 06-1: 3316275
(apply * (map ws [{:t 40 :r 233 } {:t 82 :r 1011} {:t 84 :r 1110} {:t 92 :r 1487}]))

;;; ex 06-2: 71503
(ws {:t 71530 :r 940200})

;;; ans 06-2: 27102791
(ws {:t 40828492 :r 233101111101487})
