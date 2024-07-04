(ns aoc.day19
    (:require [aoc.utils :as u]
              [instaparse.core :as p]
              [clojure.pprint :as pp]
              [clojure.core.logic.fd :as fd]))

(defn transform-rule [operand operator num result]
  (case operator
    ">" (fn [m] (if (> (get m operand) num) result))
    "<" (fn [m] (if (< (get m operand) num) result))))

(defn rule->domain [operand operator num result]
  (assoc result :domain
         (case operator
           ">" {operand (fd/interval (inc num) 4000)}
           "<" {operand (fd/interval 1 (dec num))})))

(defn parse-workflow [rule-transformer w]
  (let [parse (p/parser "workflow = label <'{'> rule (<','> rule)* <','> result <'}'>
                         label = #'[a-z]*'
                         rule = operand operator num <':'> result
                         result = label | reject | accept
                         reject = 'R'
                         accept = 'A'
                         operand = 'x' | 'm' | 'a' | 's'
                         operator = '>' | '<'
                         num = #'[0-9]+'")
        transform {:operand (fn [o] (keyword o)) :operator (fn [o] o) :num (fn [n] (Long/parseLong n))
                   :label (fn [l] (keyword l)) :reject (fn [r] :reject) :accept (fn [a] :accept)
                   :result (fn [r] {:result r})
                   :rule rule-transformer
                   :workflow (fn [l & rs] {(keyword l) (vec rs)})}]
    (->> w parse (p/transform transform))))

(defn eval-rules
  ([m rs] (eval-rules m rs :in))
  ([m rs l]
   (letfn [(eval-rule [m [r & rs]] (if (empty? rs) r (let [res (r m)] (if res res (recur m rs)))))]
     (let [res (keyword (:result (eval-rule m (l rs))))]
       (case res :accept :accept :reject :reject (recur m rs res))))))

(defn parse-part [p]
  (let [parse (p/parser
               "part = <'{'> <'x'> <'='> num <','> <'m'> <'='> num <','> <'a'> <'='> num <','> <'s'> <'='> num <'}'>
                num = #'[0-9]+'")
        transform {:num (fn [n] (Long/parseLong n)) :part (fn [x m a s] {:x x :m m :a a :s s})}]
    (->> p parse (p/transform transform))))

(defn domain->combinations [d]
  (letfn [(interval-count [i] (let [[l u] (fd/bounds i)] (inc (- u l))))]
    (apply * (map interval-count (vals d)))))

(defn split-domain [domain rule]
  (let [[k i] (first rule)]
    [(assoc domain k (fd/intersection* (k domain) i)) (assoc domain k (fd/difference* (k domain) i))]))

(defn rating-sum [rules parts]
  (apply + (flatten (map vals (filter (fn [p] (= (eval-rules p rules) :accept)) parts)))))

;;; ex 19-1: 19114
(def example-rules (apply merge (map (partial parse-workflow transform-rule) (first (u/read-paras "example19")))))
(def example-parts (map parse-part (second (u/read-paras "example19"))))
(rating-sum example-rules example-parts)

;;; ans 19-1: 333263
(def input-rules (apply merge (map (partial parse-workflow transform-rule) (first (u/read-paras "input19")))))
(def input-parts (map parse-part (second (u/read-paras "input19"))))
(rating-sum input-rules input-parts)

(defn next-tasks [workflows task]
  (let [{u :universe w :workflow} task]
    (if (empty? w) []
        (let [step (first w)]
          (if-let [d (:domain step)]
            (let [[y n] (split-domain u d)]
              [{:universe y :workflow ((:result step) workflows)} {:universe n :workflow (rest w)}])
            [{:universe u :workflow ((:result step) workflows)}])))))

(defn next-count [task]
  (let [{u :universe w :workflow} task]
    (if (empty? w) 0
        (let [step (first w)]
          (case (:result step)
            :accept
            (domain->combinations (if (:domain step) (first (split-domain u (:domain step))) u)) 0)))))

(def universe (let [u (fd/interval 1 4000)] {:x u :m u :a u, :s u}))

(defn count-combinations
  ([workflows] (count-combinations workflows [{:universe universe :workflow (:in workflows)}] 0))
  ([workflows tasks c]
   (if (empty? tasks) c
       (recur workflows
              (apply (partial conj (pop tasks)) (next-tasks workflows (peek tasks)))
              (+ c (next-count (peek tasks)))))))

;;; ex 19-2: 167409079868000
(def example-domains (apply merge (map (partial parse-workflow rule->domain) (first (u/read-paras "example19")))))
(count-combinations example-domains)

;; ans 19-2: 130745440937650
(def input-domains (apply merge (map (partial parse-workflow rule->domain) (first (u/read-paras "input19")))))
(count-combinations input-domains)
