(ns year2025.day03  
  (:require [clojure.string :as str]))

;; return the index of the maximum value
;; in the sequence xs between begin (included)
;; index and end (not included) index
(defn find-idx-max [xs begin end]
  (:max-idx
   (reduce
    (fn [acc idx]
      (if (< (:cur-max acc) (nth xs idx))
        {:cur-max (nth xs idx) :max-idx idx}
        acc))
    {:cur-max (nth xs begin) :max-idx begin}
    (range (inc begin) end))))

(defn to-int [xs]
  (reduce 
   (fn [acc val] (+ (* 10 acc) val))
   0
   xs
   ))

;; recursive function computing the index of the max value
;; in the located after the previously computed index
;; returning the list of indexes
(defn greatest-joltage-indexes [bank indexes remaining-batteries]
  (if (= 0 remaining-batteries)
    indexes
    (let [last-idx (if (empty? indexes) -1 (last indexes))
          next-idx (find-idx-max 
                    bank 
                    (inc last-idx) 
                    (- (inc (count bank)) remaining-batteries))
          ]
      (greatest-joltage-indexes bank (conj indexes next-idx) (dec remaining-batteries)))
    )
  )

(defn greatest-joltage [bank nb-batteries]
  (to-int (map
   (fn [idx] (nth bank idx))
   (greatest-joltage-indexes bank [] nb-batteries)
   )))

(defn parse-bank [line]
  (reduce 
   (fn [acc c] (conj acc (Character/digit c 10)))
   []
   line))

(defn parse-banks [input]
  (map parse-bank (str/split-lines input)))

(defn solve [input nb-batteries]
  (reduce
   +
   (map
    (fn [bank] (greatest-joltage bank nb-batteries))
    (parse-banks input))))

(defn part-1 [input]
  (solve input 2))

(defn part-2 [input]
  (solve input 12))
