(ns year2025.day11
  (:require [toolbox.parsing :as parsing]
            [year2025.day02 :refer [sliding-window]]))

(defn parse-device [line]
  (let [splitted (parsing/split line #": ")]
    {:device-name (first splitted)
     :outputs (parsing/split (second splitted) #" ")}))

(defn parse-devices [input]
  (map parse-device (parsing/parse-lines input)))

(defn parse-graph [input]
  (reduce
   (fn [graph device] (assoc graph (:device-name device) (:outputs device)))
   {}
   (parse-devices input)))

;; DAG structure
;; => dynamic programming
(defn dfs [graph current end memo]
  (if (= current end)
    [1 memo]
    (if (contains? memo current)
      [(get memo current) memo]
      (let [[counts memo]
            (reduce
             (fn [[total memo] neigh]
               (let [[count memo] (dfs graph neigh end memo)]
                 [(+ total count) memo]))
             [0 memo]
             (get graph current))]
        [counts (assoc memo current counts)]))))

(defn number-path [graph milestones]
  (reduce 
   *
   1
   (map 
    (fn [[source dest]] (first (dfs graph source dest {})))
    (sliding-window milestones))))

(defn part-1 [input]
  (number-path (parse-graph input) ["you" "out"]))

(defn part-2 [input]
  (let [graph (parse-graph input)]
    (+
     (number-path graph ["svr" "fft" "dac" "out"])
     (number-path graph ["svr" "dac" "fft" "out"]))))