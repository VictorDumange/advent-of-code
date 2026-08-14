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

(defn extract-button-indexes [inner]
  (map parsing/parse-int (parsing/split inner #",")))

(defn parse-wiring-schematic [parenthese indicator-size]
  (let [inner (-> parenthese inner-expr extract-button-indexes)]
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

(defn part-1 [input]
  (let [machines (parse-machines input)]
    (reduce + (map fewest-presses machines))))



