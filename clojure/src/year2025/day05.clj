(ns year2025.day05
  (:require [toolbox.parsing :as parsing]))

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

(defn count-fresh [database]
  (count (get-fresh database)))

(defn part-1 [input]
  (count-fresh (parse-database input)))