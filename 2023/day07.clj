(ns aoc.day07
  (:require [aoc.utils :as u]
            [instaparse.core :as p]))

(defn read-hand [->strength line]
  (let [parse (p/parser
               "hand = card card card card card <' '> bid
                card = #'[2-9AKQJT]'
                bid = #'[0-9]+'")
        transform {:bid (fn [b] {:bid (Long/parseLong b)})
                   :hand (fn [& hand]
                           (let [cards (map second (butlast hand))]
                             (merge {:cards cards} (last hand) {:strength (->strength cards)})))}]
    (p/transform transform (parse line))))

(defn strength [cards]
  (let [fs (vals (frequencies cards))
        ffs (frequencies fs)]
    (cond
      (= (apply max fs) 5) 5
      (= (apply max fs) 4) 4
      (and (some #{3} fs) (some #{2} fs)) 3.5
      (some #{3} fs) 3
      (= (ffs 2) 2) 2
      (some #{2} fs) 1
      :else 0)))

(defn best-joker [cards]
  (let [fs (dissoc (frequencies cards) "J")
        max-freq (if (empty? fs) "A" (apply max (vals fs)))]
    (last (sort compare-card-j (filter #(= (fs %) max-freq) (keys fs))))))

(defn strength-j [cards]
  (strength (replace {"J" (best-joker cards)} cards)))

(defn ->compare [worst-to-best c d]
  (let [strength-map (apply merge (map-indexed (fn [i c] {c i}) (map str (seq worst-to-best))))]
    (compare (strength-map c) (strength-map d))))

(defn compare-card [c d]
  (->compare "23456789TJQKA" c d))

(defn compare-card-j [c d]
  (->compare "J23456789TQKA" c d))

(defn compare-cards [->compare-card c d]
  (let [card-comparison (->compare-card (first c) (first d))]
    (if (zero? card-comparison)
      (recur ->compare-card (rest c) (rest d))
      card-comparison)))

(defn compare-hands [->strength ->compare-card c d]
  (let [strength-comparison (compare (->strength (:cards c)) (->strength (:cards d)))]
    (if (zero? strength-comparison)
      (compare-cards ->compare-card (:cards c) (:cards d))
      strength-comparison)))

(defn winnings [input ->strength ->compare-card]
  (->> input
       u/read-lines
       (map #(read-hand ->strength %))
       (sort #(compare-hands ->strength ->compare-card %1 %2))
       (map-indexed (fn [r cs] (* (inc r) (:bid cs))))
       (apply +)))

;;; ex 07-1: 6440
(winnings "example07" strength compare-card)

;; ans 07-2: 250602641
(winnings "input07" strength compare-card)

;;; ex 07-2: 5905
(winnings "example07" strength-j compare-card-j)

;; ans 07-2: 251037509
(winnings "input07" strength-j compare-card-j)
