(ns year2025.day09
  (:require [toolbox.parsing :as parsing]))

(defn parse-tile [line]
  (let [parsed (map parsing/parse-int (parsing/split line #","))]
    {:x (first parsed)
     :y (last parsed)}))

(defn parse-tiles [input]
  (map parse-tile (parsing/parse-lines input)))

(defn pairs [n]
  (for [i (range n)
        j (range (inc i) n)]
    [i j]))

(defn tile-pairs [tiles]
  (map 
   (fn [[i j]] 
     [(nth tiles i) 
      (nth tiles j)])
   (pairs (count tiles))))

(defn rectangle-area [pair]
  (let [nw (first pair)
        se (last pair)] 
    (*
     (inc (abs (- (:x nw) (:x se))))
     (inc (abs (- (:y nw) (:y se)))))))

(defn greatest-rectangle [pairs]
  (reduce 
   (fn [max-area pair] (max max-area (rectangle-area pair))) 
   (rectangle-area (first pairs))
   (rest pairs)))

(defn part-1 [input]
  (-> input
      parse-tiles
      tile-pairs
      greatest-rectangle))