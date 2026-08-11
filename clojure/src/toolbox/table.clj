(ns toolbox.table
  (:require [toolbox.parsing :as parsing]))

(defn at [table row col]
  (nth (nth table row) col))

(defn nb-cols [table]
  (reduce 
   (fn [acc row] (max acc (count row)))
   (count (first table))
   (rest table)
   ))

(defn get-row [table n]
  (nth table n))

(defn size-nth-row [table n]
  (count (get-row table n)))

(defn nb-rows [table]
  (count table))

(defn nth-column [table col]
  (reduce 
   (fn [acc row] 
     (if (< col (size-nth-row table row))
       (conj acc (at table row col))
       acc) 
     )
   []
   (range 0 (nb-rows table))
   ))

(defn cols [table]
  (reduce 
   (fn [acc c] (conj acc (nth-column table c)))
   []
   (range 0 (nb-cols table))
   ))

(defn rotate [table]
  (map 
   (fn [col] (nth-column table col))
   (range 0 (nb-cols table))
   ))

(defn map-table [f table] 
  (reduce 
   (fn [acc row] (conj acc (map f row)))
   []
   table
   ))

(defn- parse-row [row]
  (parsing/split (parsing/trim row) #"\s+"))

(defn parse-table [input]
  (map parse-row (parsing/parse-lines input)))