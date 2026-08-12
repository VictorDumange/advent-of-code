(ns year2025.day08
  (:require [toolbox.dsu :as dsu]
            [toolbox.parsing :as parsing]))

(defn distance-1d [x y]
  (abs (- x y)))

(defn square [x]
  (* x x))

(defn distance [p1 p2]
  (->> (map distance-1d p1 p2)
       (map square)
       (reduce +)
       Math/sqrt))

(defn pairs [points]
  (let [points (vec points)]
    (for [i (range (count points))
          j (range (inc i) (count points))]
      [(points i) (points j)])))

(defn closest-pairs [points]
  (sort-by
   (fn [[p1 p2]]
     (distance p1 p2))
   (pairs points)))

(defn junctions [points]
  (reduce
   (fn [acc p] (dsu/make-set acc p))
   (dsu/make-dsu)
   points))

(defn connect [circuits pair]
  (dsu/union circuits (first pair) (last pair)))

(defn make-circuits [points n]
  (reduce
   (fn [circuits pair] (connect circuits pair))
   (junctions points)
   (take n (closest-pairs points))))

(defn parse-line [line]
  (map parsing/parse-long (parsing/split line #",")))

(defn parse-points [input]
  (map
   parse-line
   (parsing/parse-lines input)))

(defn circuits-sizes [circuits]
  (let [reprentants (dsu/groups circuits)]
    (map
     (fn [rep] (dsu/get-size circuits rep))
     reprentants)))

(defn find-k-largest [values k]
  (->> values
       sort
       reverse
       (take k)))

(defn part-1 [input n]
  (let [sizes (-> input
                  parse-points
                  (make-circuits n)
                  circuits-sizes)]
    (reduce * 1 (find-k-largest sizes 3))))

(defn one-large-circuit? [circuits]
  (= (dsu/nb-groups circuits) 1))

(defn connect-until-large [circuits pairs]
  (let [next-pair (first pairs)
        updated-circuits (connect circuits next-pair)]
    (if (one-large-circuit? updated-circuits) 
      next-pair
      (recur updated-circuits (next pairs))))) 

(defn last-pair-to-connect [points]
  (let [pairs (closest-pairs points)
        circuits (junctions points)]
    (connect-until-large circuits pairs)))

(defn multiply-x [pair]
  (let [j1 (first pair)
        j2 (last pair)]
    (* (first j1) (first j2))))

(defn part-2 [input]
  (-> input 
      (parse-points)
      (last-pair-to-connect)
      (multiply-x)))