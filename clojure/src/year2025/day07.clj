(ns year2025.day07
  (:require
   [toolbox.grid :as grid]))

;; pos is only the col in the grid
(defn splitted? [grid step beam-pos]
  (= (grid/at grid {:x step :y beam-pos}) \^))

;; state =  pos of beam + nb spliited beam
;; returns state
(defn split-beam [state beam-pos]
  (let [beam-positions (:beam-positions state)]
    {:nb-splitted-beam (inc (:nb-splitted-beam state))
     :beam-positions (-> beam-positions
                         (disj beam-pos)
                         (conj (dec beam-pos))
                         (conj (inc beam-pos)))}))

(defn update-state [grid step beam-pos state]
  (if (splitted? grid step beam-pos)
    (split-beam state beam-pos)
    state))

(defn transition [grid step state]
  (reduce
   (fn [updated-state beam-pos] 
     (update-state 
      grid 
      step 
      beam-pos
      updated-state))
   state
   (:beam-positions state)))

(defn find-start [grid]
  (some
   (fn [col]
     (when (= (grid/at grid {:x 0 :y col}) \S)
       col))
   (range (grid/nb-cols grid))))

(defn init-state [grid]
  {:nb-splitted-beam 0
   :beam-positions #{(find-start grid)}})

(defn steps [grid]
  (range 1 (grid/nb-rows grid)))

(defn find-nb-splits [grid]
  (:nb-splitted-beam
   (reduce
    (fn [state step] (transition grid step state))
    (init-state grid)
    (steps grid))))

(defn part-1 [input]
  (find-nb-splits (grid/parse-grid input)))