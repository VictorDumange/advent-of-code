(ns year2025.day04
  (:require [clojure.string :as str]))

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
    (str/split-lines input))))

(defn paper-roll? [grid pos]
  (not= \. (at grid pos)))

(defn adjacent-rolls [grid pos] 
  (filter
   (fn [neigh] (paper-roll? grid neigh))
   (neighbors grid pos directions8)))

(defn count-adjacent-rolls [grid pos]
  (count (adjacent-rolls grid pos)))

(defn paper-roll-accessible? [grid pos]
  (<
   (count-adjacent-rolls grid pos)
   4))

(defn paper-roll-positions [grid]
  (filter
   (fn [pos] (paper-roll? grid pos))
   (positions grid)))

(defn accessible-paper-roll [grid]
  (filter
   (fn [pos] (paper-roll-accessible? grid pos))
   (paper-roll-positions grid)))

(defn count-accessible-paper-roll [grid]
  (count (accessible-paper-roll grid)))

(defn part-1 [input]
  (count-accessible-paper-roll (parse-grid input)))

(defn pop-remaining [state]
  {:remaining (rest (:remaining state))
   :removed (:removed state)})

(defn keep-roll [updated-state remaining]
  {:remaining (conj (:remaining updated-state) remaining)
   :removed (:removed updated-state)})

(defn remove-roll [updated-state removed]
  {:remaining (:remaining updated-state)
   :removed (conj (:removed updated-state) removed)})

(defn is-removed? [pos removed-rolls]
  (some 
   (fn [removed] (same-positions? pos removed))
   removed-rolls))

;; neighbors that are not removed
(defn remaining-neighbors [grid pos removed]
  (filter
   (fn [pos] (not (is-removed? pos removed)))
   (adjacent-rolls grid pos)) )

(defn paper-roll-removable? [grid cur removed]
  (<
   (count (remaining-neighbors grid cur removed))
   4))

(defn transition [grid state updated-state]
  (if (empty? (:remaining state))
    updated-state
    (let [candidate (first (:remaining state))] 
      (if (paper-roll-removable? grid candidate (:removed state)) 
        (recur grid (pop-remaining state) (remove-roll updated-state candidate)) 
        (recur grid (pop-remaining state) (keep-roll updated-state candidate))))))

(defn improve? [state updated-state]
  (< (count (:remaining updated-state)) (count (:remaining state))))

(defn init-next-state [state]
  {:remaining [] :removed (:removed state)})

(defn removable-rolls [grid state]
  (let [updated-state (transition grid state (init-next-state state))]
    (if (improve? state updated-state)
      (recur grid updated-state)
      updated-state)))

(defn init-state [grid] 
  (let [rolls (paper-roll-positions grid)]
    {:remaining rolls :removed []}))

(defn count-removable-paper-rolls [grid]
  (count (:removed (removable-rolls grid (init-state grid)))))

(defn part-2 [input]
  (count-removable-paper-rolls (parse-grid input)))