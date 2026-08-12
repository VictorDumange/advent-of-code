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

(defn counts-splits [grid]
  (:nb-splitted-beam
   (reduce
    (fn [state step] (transition grid step state))
    (init-state grid)
    (steps grid))))

(defn ultimate-step? [grid step]
  (= (dec (grid/nb-rows grid)) step))

(defn will-be-splitted? [grid step beam-pos]
  (splitted? grid (inc step) beam-pos))

(declare timelines-at)

(defn continue-straight [grid step beam-pos memo]
  (let [[timelines memo]
        (timelines-at grid (inc step) beam-pos memo)]
    [timelines
     (assoc memo [step beam-pos] timelines)]))

(defn split [grid step beam-pos memo]
  (let [[left memo]
        (timelines-at grid (inc step) (dec beam-pos) memo)

        [right memo]
        (timelines-at grid (inc step) (inc beam-pos) memo)

        timelines (+ left right)]

    [timelines
     (assoc memo [step beam-pos] timelines)]))

;; recursive dynamic programming
;; uses memoization
(defn timelines-at [grid step beam-pos memo]
  (cond
    (ultimate-step? grid step)
    [1 memo]

    (contains? memo [step beam-pos])
    [(get memo [step beam-pos]) memo]

    (will-be-splitted? grid step beam-pos)
    (split grid step beam-pos memo)

    :else
    (continue-straight grid step beam-pos memo)))

(defn counts-timelines [grid]
  (first (timelines-at grid 0 (find-start grid) {})))

(defn part-1 [input]
  (counts-splits (grid/parse-grid input)))

(defn part-2 [input]
  (counts-timelines (grid/parse-grid input)))