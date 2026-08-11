(ns year2025.day06
  (:require [toolbox.table :as table]
            [toolbox.parsing :as parsing]))

(defn find-operator-indexes [last-line]
  (reduce
   (fn [indexes i]
     (if (= (nth last-line i) \space)
       indexes
       (conj indexes i)))
   []
   (range 0 (count last-line))))

(defn find-splitting-indexes [last-line]
  (map dec (rest (find-operator-indexes last-line))))

(defn- split-at-indexes* [s indexes start result]
  (if-let [index (first indexes)]
    (recur s
           (rest indexes)
           (inc index)
           (conj result (subs s start index)))
    (conj result (subs s start))))


(defn split-at-indexes [s indexes]
  (split-at-indexes* s indexes 0 []))

(defn parse-rows [input-rows]
  (let [splitting-indexes (find-splitting-indexes (last input-rows))]
    (map
     (fn [row] (split-at-indexes row splitting-indexes))
     input-rows)))

(defn parse-problems [input]
  (table/rotate (parse-rows (parsing/parse-lines input))))

;; " 12" should be considered as 12
(defn decode-operand [operand]
  (parsing/parse-long (parsing/trim operand)))

(defn decode-operands [encoded-operands]
  (map decode-operand encoded-operands))

(defn decode-operator [encoded-operator]
  (if (= (parsing/trim encoded-operator) "+")
    +
    *))

(defn compute [operands operator]
  (reduce
   (fn [acc operand] (operator acc operand))
   (if (= operator +) 0 1)
   operands))

;; transform -> decode -> compute
(defn solve-problem [pb transform]
  (let [operands (decode-operands (transform (butlast pb)))
        operator (decode-operator (last pb))]
    (compute operands operator)))

;; transform is a function applied to encoded operands
;; part-1 : identity
;; part-2 : rotate-operands
(defn individual-anwsers [problems transform]
  (map (fn [pb] (solve-problem pb transform)) problems))

(defn grand-total [problems transformation]
  (reduce
   +
   (individual-anwsers problems transformation)))

;; " 123" -> [\space \1 \2 \3]
(defn make-row [operand]
  (reduce 
   (fn [acc c] (conj acc c))
   []
   operand
   ))

(defn make-table [operands]
  (map make-row operands))

;; vector of char -> string
(defn make-operand [row]
  (reduce
   (fn [acc c] (str acc c))
   ""
   row))

;; operands -> table
;; rotate table
;; rotated table -> operands
(defn rotate-operands [operands]
  (map
   make-operand
   (table/rotate (make-table operands))))

(defn part-1 [input]
  (grand-total (parse-problems input) identity))

(defn part-2 [input]
  (grand-total (parse-problems input) rotate-operands))

