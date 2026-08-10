(ns year2025.day04
  (:require [clojure.string :as str]))

(def directions8
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= [dx dy] [0 0])]
    {:x dx :y dy}))

(defn nb-rows [grid]
  (count grid))

(defn nb-cols [grid]
  (count (first grid)))

;; unsafe
(defn at [grid pos]
  (nth (nth grid (:x pos)) (:y pos)))

(defn translate [pos dir]
  {:x (+ (:x pos) (:x dir))
   :y (+ (:y pos) (:y dir))})

(defn is-inside? [grid pos]
  (let [rows (nb-rows grid)
        cols (nb-cols grid)
        x (:x pos)
        y (:y pos)]
    (and 
     (<= 0 x) 
     (< x rows)
     (<= 0 y) 
     (< y cols)))
  )

(defn neighbors [grid pos directions]
  (filter
   (fn [neighbor] 
     (is-inside? grid neighbor))
   (map 
    (fn [dir] (translate pos dir)) 
    directions)))

(defn str-to-vec [input f]
  (reduce
   (fn [acc c] (conj acc (f c)))
   []
   input))

(defn positions [grid]
  (for [row (range (nb-rows grid))
        col (range (nb-cols grid))]
    {:x row :y col}))

(defn parse-grid
  ([input]
   (parse-grid input identity))
  ([input f]
   (map
    (fn [line] (str-to-vec line f))
    (str/split-lines input))))

(defn paper-roll? [grid pos]
  (not= \. (at grid pos)))

(defn count-adjacent-rolls [grid pos]
  (count (filter 
          (fn [neigh] (paper-roll? grid neigh))
          (neighbors grid pos directions8))))

(defn paper-roll-accessible? [grid pos]
  (< 
   (count-adjacent-rolls grid pos)
   4))

(defn paper-roll-positions [grid]
  (filter 
   (fn [pos] (paper-roll? grid pos))
   (positions grid)))

(defn accessible-paper-roll [grid]
  (filter
   (fn [pos] (paper-roll-accessible? grid pos))
   (paper-roll-positions grid))
  )

(defn count-accessible-paper-roll [grid]
  (count (accessible-paper-roll grid)))

(defn part-1 [input]
  (count-accessible-paper-roll (parse-grid input)))

