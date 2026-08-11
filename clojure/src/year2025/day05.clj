(ns year2025.day05
  (:require [toolbox.parsing :as parsing]
            [toolbox.intervals :as intervals]))

(defn parse-id-range [line]
  (let [parsed (map 
                parsing/parse-long 
                (parsing/split line #"-"))]
    {:begin (first parsed)
     :end (last parsed)}))

(defn parse-id-ranges [first-block]
  (map 
   parse-id-range 
   (parsing/parse-lines first-block)))

(defn parse-available-ids [second-block]
  (map 
   parsing/parse-long
   (parsing/parse-lines second-block)))

(defn database-tables [input]
  (let [tables (parsing/split input #"\n\n")]
    {:fresh-table (first tables)
     :available-table (last tables)}))

(defn parse-database [input]
  (let [tables (database-tables input)
        fresh-ids (parse-id-ranges (:fresh-table tables))
        available-ids (parse-available-ids (:available-table tables))]
    {:fresh-id-ranges fresh-ids :available-ids available-ids}))

(defn included? [id id-range]
  (and 
   (>= id (:begin id-range))
   (<= id (:end id-range))))

(defn fresh? [id id-ranges]
  (some 
   (fn [id-range] 
     (included? id id-range))
   id-ranges))

(defn get-fresh [database]
  (filter 
   (fn [id] (fresh? id (:fresh-id-ranges database)))
   (:available-ids database)
   ))

;; count the number of available ingredients
;; that are fresh
(defn count-available-fresh [database]
  (count (get-fresh database)))

(defn part-1 [input]
  (count-available-fresh (parse-database input)))

(defn extract-ranges [input]
  (parse-id-ranges 
   (:fresh-table (database-tables input))))

(defn merge-overlapping [input]
  (intervals/merge input))

;; TO DO : put into toolbox.intervals
(defn range-size [id-range]
  (inc (- (:end id-range) (:begin id-range))))

;; count the number of ingredients
;; that can be fresh
(defn count-possibly-fresh [id-ranges]
  (reduce 
   +
   (map 
    range-size 
    (merge-overlapping id-ranges))))

(defn part-2 [input]
  (count-possibly-fresh (extract-ranges input)))