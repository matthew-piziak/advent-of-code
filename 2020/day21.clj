(ns aoc.day21
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as set]))

(u/read-lines "example21")

(defn parse-line [line]
  (let [[_ ingredients allergens] (re-find #"([a-z ]+) \(contains (.*)\)" line)]
    {:ingredients (into #{} (s/split ingredients #" ")), :allergens (into #{} (map s/trim (u/uncommas allergens)))}))

(defn allergen->ingredient [data allergen]
  (->> data
     (filter #(contains? (% :allergens) allergen))
     (map :ingredients)
     (apply set/intersection)))

(let [data (map parse-line (u/read-lines "input21"))]
  (->> data
   (map :allergens)
   (apply set/union)
   (map (juxt identity #(allergen->ingredient data %)))))

(def allergy-sets
  [["eggs" #{"kgbzf"}]
   ["sesame" #{"kpsdtv"}]
   ["peanuts" #{"pzmg"}]
   ["wheat" #{"qpxhfp"}]
   ["dairy" #{"fllssz"}]
   ["shellfish" #{"fvvrc"}]
   ["soy" #{"dqbjj"}]
   ["nuts" #{"zcdcdf"}]])

(def allergy-map
  [["eggs" "kgbzf"]
   ["sesame" "kpsdtv"]
   ["peanuts" "pzmg"]
   ["wheat" "qpxhfp"]
   ["dairy" "fllssz"]
   ["shellfish" "fvvrc"]
   ["soy" "dqbjj"]
   ["nuts" "zcdcdf"]])

;;; ans 21-2: fllssz,kgbzf,zcdcdf,pzmg,kpsdtv,fvvrc,dqbjj,qpxhfp
(s/join "," (map second (sort-by first allergy-map)))

(def allergy-unions
  [#{"kgbzf"}
   #{"kgbzf" "zcdcdf" "kpsdtv"}
   #{"pzmg" "dqbjj"}
   #{"kgbzf" "fllssz" "qpxhfp" "fvvrc" "zcdcdf"}
   #{"kgbzf" "fllssz"}
   #{"kgbzf" "fllssz" "fvvrc" "zcdcdf"}
   #{"kgbzf" "fllssz" "dqbjj"}
   #{"kgbzf" "fllssz" "zcdcdf"}])

(def dat (map parse-line (u/read-lines "input21")))

(def bad-ings #{"kgbzf" "fllssz" "qpxhfp" "pzmg" "fvvrc" "dqbjj" "zcdcdf" "kpsdtv"})

;;; ans 21-1: 1885
(apply + (map count (map #(set/difference % bad-ings) (map :ingredients (map parse-line (u/read-lines "input21"))))))

;;; dairy
;; #{"mxmxvkd"} is dairy
;; "sqjhc" is fish
;; "fvjkl" is soy
(set/intersection #{"fvjkl" "trh" "sbzzf" "mxmxvkd"} #{"sqjhc" "sbzzf" "mxmxvkd"})

;;; dairy
(set/intersection #{"fvjkl" "trh" "sbzzf" "mxmxvkd"} #{"fvjkl" "trh" "sbzzf" "mxmxvkd"})

;; ONE OF "sqjhc" "nhms" "kfcds" "mxmxvkd" is "dairy"
;; ONE OF "sqjhc" "nhms" "kfcds" "mxmxvkd" is "fish"
;; ONE OF "fvjkl" "trh" "sbzzf" "mxmxvkd" is "dairy"
;; ONE OF "sqjhc" "fvjkl" is "soy"
;; ONE OF "sqjhc" "sbzzf" "mxmxvkd" is "fish"

;;; Why can't they contain allergens?
;; kfcds and nhms
;; sbzzf:
;; trh:
