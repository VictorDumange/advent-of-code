(ns year2025.day10
  (:require [toolbox.bits :as bits]
            [toolbox.graph :as graph]
            [toolbox.parsing :as parsing]))

;; "[.##.]" -> ".##."
(defn inner-expr [wrapped-expr]
  (subs wrapped-expr 1 (dec (count wrapped-expr))))

;; ".##." -> [1 2]
(defn on-indexes [indicator]
  (reduce 
   (fn [indexes i] (if (= (nth indicator i) \#)
                     (conj indexes i)
                     indexes)) 
   []
   (range 0 (count indicator))))

;; bit index is right to left
(defn bit-index [index size]
  (- size (inc index)))

;; [1 2] 4 -> 0110 
(defn to-bits [indexes size]
  (reduce 
   (fn [acc idx] (bits/toggle acc (bit-index idx size)))
   0
   indexes))

(defn parse-indicator [inner]
  (to-bits (on-indexes inner) (count inner)))

(defn extract-values [expr]
 (let [splitted (-> expr inner-expr (parsing/split #","))]
   (map parsing/parse-int splitted)))

(defn extract-button-indexes [expr]
  (extract-values expr))

(defn parse-wiring-schematic [parenthese indicator-size]
  (let [inner (extract-button-indexes parenthese)]
    (to-bits inner indicator-size)))

(defn parse-wiring-schematics [parentheses indicator-size]
  (map 
   (fn [parenthese] (parse-wiring-schematic parenthese indicator-size))
   parentheses))

(defn parse-machine [line]
  (let [splitted (parsing/split line #" ")
        lhs (inner-expr (first splitted))
        parentheses (rest (butlast splitted))
        indicator (parse-indicator lhs)
        schematics (parse-wiring-schematics parentheses (count lhs))]
    {:indicator indicator :buttons schematics}))

(defn press-button [indicator button]
  (bit-xor indicator button))

(defn neighbors [machine indicator]
  (map 
   (fn [button] (press-button indicator button)) 
   (:buttons machine)))

(defn parse-machines [input]
  (map parse-machine (parsing/parse-lines input)))

(defn off-indicator [] 0)

(defn goal? [machine indicator]
  (= (:indicator machine) indicator))

(defn fewest-presses [machine] 
  (let [path (graph/path (graph/bfs 
               machine 
               (off-indicator)
               neighbors 
               goal?))]
    (graph/path-length path)))

(defn initial-counters [machine]
  (vec (repeat (count (:joltage machine)) 0)))

(defn press-button-joltage [counters button]
  (reduce
   (fn [counters index]
     (update counters index inc))
   counters
   button))

(defn adjacent-counters [machine counters]
  (map 
   (fn [button] (press-button-joltage counters button))
   (:buttons machine)))

(defn valid-counters? [machine counters]
  (every?
   (fn [[counter target]]
     (<= counter target))
   (map vector counters (:joltage machine))))

(defn neighbors-counters [machine current-counters]
  (filter 
   (fn [counters] (valid-counters? machine counters))
   (adjacent-counters machine current-counters)))

(defn joltage-configured? [machine counters]
  (= (:joltage machine) counters))

(defn fewest-joltage-presses [machine]
  (let [path (graph/path (graph/bfs
                          machine 
                          (initial-counters machine) 
                          neighbors-counters
                          joltage-configured?
                          ))]
    (graph/path-length path)))

(defn parse-joltage [rhs]
  (extract-values rhs))

(defn parse-button [parenthese]
  (extract-values parenthese))

(defn parse-buttons [parentheses]
  (map parse-button parentheses))

(defn parse-joltage-machine [line]
  (let [splitted (parsing/split line #" ")
        joltage (parse-joltage (last splitted))
        buttons (parse-buttons (rest (butlast splitted)))]
    {:buttons buttons :joltage joltage}))

(defn parse-joltage-machines [input]
  (map parse-joltage-machine (parsing/parse-lines input)))

(defn part-1 [input]
  (let [machines (parse-machines input)]
    (reduce + (map fewest-presses machines))))

(defn part-2 [input]
    (let [machines (parse-joltage-machines input)]
    (reduce + (map fewest-joltage-presses machines))))



