(ns toolbox.grid 
  (:require [toolbox.parsing :as parsing]))

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
     (< y cols))))

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

(defn same-positions? [pos1 pos2]
  (and
   (= (:x pos1) (:x pos2))
   (= (:y pos1) (:y pos2))))

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
    (parsing/parse-lines input))))