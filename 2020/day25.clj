(ns aoc.day25)

(def example-card-public-key 5764801) ; the subject number of 7 transformed with the card's secret loop size
(def example-door-public-key 17807724) ; the subject number of 7 transformed with the door's secret loop size

(def subject-number 7)

(defn transform [sub n]
  (rem (* sub n) 20201227))

(defn loop-size [public-key]
  (.indexOf (iterate (partial transform subject-number) 1) public-key))

(def example-card-loop-size (loop-size example-card-public-key))
(def example-door-loop-size (loop-size example-door-public-key))

(defn enc-key [sub loop-size]
  (last (take (inc loop-size) (iterate (partial transform sub) 1))))

(enc-key example-card-public-key example-door-loop-size)
(enc-key example-door-public-key example-card-loop-size)

(def input-card-public-key 8987316)
(def input-door-public-key 14681524)
(def input-card-loop-size (loop-size input-card-public-key))
(def input-door-loop-size (loop-size input-door-public-key))

(enc-key input-card-public-key input-door-loop-size)
(enc-key input-door-public-key input-card-loop-size)
