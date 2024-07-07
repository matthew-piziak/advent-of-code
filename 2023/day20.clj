(ns aoc.day20
  (:require [aoc.utils :as u]
            [instaparse.core :as p]
            [clojure.pprint :as pp]))

(defn parse-module [c]
  (let [parse (p/parser "module = src <' -> '> name (<', '> name)*
                         src = cast | flfl | conj
                         cast = name
                         flfl = <'%'> name
                         conj = <'&'> name
                         name = #'[a-z]*'")
        transform {:module (fn [[t n] & ds] {n {:type t :dests(vec ds)}}) :name (fn [n] (keyword n)) :src (fn [s] s)}]
    (->> c parse (p/transform transform))))

(defn parse-config [in]
  (->> in u/read-lines (map parse-module) (into {})))

(defn flfls [c]
  (into {} (map (fn [[k v]] [k :off]) (filter (fn [[_ v]] (= (:type v) :flfl)) c))))

(defn inputs [c conj]
  (into {} (map (fn [[k v]] [k :lo]))(filter (fn [[_ v]] (some #{conj} (:dests v))) c)))

(defn conjs [c]
  (into {} (map (fn [[k v]] [k (inputs c k)]) (filter (fn [[_ v]] (= (:type v) :conj)) c))))

(defn new-pulse-counts [[lo-pulses hi-pulses] p]
  [(case p :lo (inc lo-pulses) lo-pulses) (case p :hi (inc hi-pulses) hi-pulses)])

(def button-press (u/queue [{:dest :broadcaster :pulse :lo :src :button}]))

(defn send-pulses
  ([c] (send-pulses c button-press (flfls c) (conjs c) 1 [0 0]))
  ([c q flfls conjs presses pulse-counts]
   (cond
     (> presses 1000) pulse-counts
     (empty? q) (recur c button-press flfls conjs (inc presses) pulse-counts)
     :otherwise
     (let [{dest :dest p :pulse src :src} (peek q)
           m (dest c)]
       (case (:type m)
         :cast
         (recur c
                (reduce (fn [q t] (conj q t)) (pop q) (map (fn [d] {:dest d :pulse p :src :broadcaster}) (:dests m)))
                flfls conjs presses (new-pulse-counts pulse-counts p))
         :flfl
         (recur c
                (case p
                  :hi (pop q)
                  (reduce (fn [q t] (conj q t)) (pop q)
                          (map (fn [d] {:dest d :pulse (case (dest flfls) :off :hi :lo) :src dest}) (:dests m))))
                (case p :hi flfls (assoc flfls dest (case (dest flfls) :on :off :on)))
                conjs presses (new-pulse-counts pulse-counts p))
         :conj
         (let [new-conjs (assoc-in conjs [dest src] p)]
           (recur c
                  (reduce
                   (fn [q t] (conj q t)) (pop q)
                   (map
                    (fn [d] {:dest d :pulse (if (every? #(= :hi %) (vals (dest new-conjs))) :lo :hi) :src dest})
                    (:dests m)))
                  flfls new-conjs presses (new-pulse-counts pulse-counts p)))
         (recur c (pop q) flfls conjs presses (new-pulse-counts pulse-counts p)))))))

;; ex 20-1: 32000000, 11687500
(apply * (send-pulses (parse-config "example20")))
(apply * (send-pulses (parse-config "extra20")))

;;; ans 20-1: 832957356
(apply * (send-pulses (parse-config "input20")))

(defn presses-until-lo
  ([c t] (presses-until-lo c button-press (flfls c) (conjs c) 1 t))
  ([c q flfls conjs presses t]
   (cond
     (empty? q) (recur c button-press flfls conjs (inc presses) t)
     :otherwise
     (let [{dest :dest p :pulse src :src} (peek q)
           m (dest c)]
       (if (and (= dest t) (= p :lo)) presses
           (case (:type m)
             :cast
             (recur c
                    (reduce (fn [q t] (conj q t)) (pop q) (map (fn [d] {:dest d :pulse p :src :broadcaster}) (:dests m)))
                    flfls conjs presses t)
             :flfl
             (recur c
                    (case p
                      :hi (pop q)
                      (reduce (fn [q t] (conj q t)) (pop q)
                              (map (fn [d] {:dest d :pulse (case (dest flfls) :off :hi :lo) :src dest}) (:dests m))))
                    (case p :hi flfls (assoc flfls dest (case (dest flfls) :on :off :on)))
                    conjs presses t)
             :conj
             (let [new-conjs (assoc-in conjs [dest src] p)]
               (recur c
                      (reduce
                       (fn [q t] (conj q t)) (pop q)
                       (map
                        (fn [d] {:dest d :pulse (if (every? #(= :hi %) (vals (dest new-conjs))) :lo :hi) :src dest})
                        (:dests m)))
                      flfls new-conjs presses t))
             (recur c (pop q) flfls conjs presses t)))))))

;;; ans 20-2: 240162699605221
(apply * (map (partial presses-until-lo (parse-config "input20")) [:xj :qs :kz :km]))
