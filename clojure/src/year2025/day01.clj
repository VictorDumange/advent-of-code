(ns year2025.day01)

;; custom reduce function
(defn red [f seq acc]
  (if (empty? seq)
    acc
    (recur
     f
     (rest seq)
     (f acc (first seq)))))

;; split utils
;; only works when delimiter is a char
(defn split [input delimiter]
  (let [res (red
             (fn [acc char]
               (if (= char delimiter)
                 [(conj (first acc) (last acc)) ""]
                 [(first acc) (str (last acc) char)]))
             input
             [[] ""])]
    (conj (first res) (last res))))

(defn get-lines [input]
  (split input \newline))

(defn char-to-digit [c]
  (Character/digit c 10))

(defn str-to-int [s]
  (red
   (fn [acc c] (+ (char-to-digit c) (* 10 acc)))
   s
   0))

(defn direction [rot]
  (if (= (first rot) \L)
    -
    +))

(defn distance [rot]
  (str-to-int (subs rot 1)))

(defn translation [dial rot]
  ((direction rot) dial (distance rot)))

(defn count-right-crossings [start distance]
  (quot (+ start distance) 100))

(defn count-zero-crossings [start rot]
  (if (= (direction rot) +)
    (count-right-crossings start (distance rot))
    (count-right-crossings (mod (- start) 100) (distance rot))))

(defn rotate [dial rot]
  (let [translated-dial (translation dial rot)]
    {:updated-dial (mod translated-dial 100)
     :nb-rotatation (count-zero-crossings dial rot)}))

(defn count-zero-1 [cur updated-rot]
  (if (= 0 (:updated-dial updated-rot))
    (inc cur)
    cur))

(defn count-zero-2 [cur updated-rot]
  (+ cur (:nb-rotatation updated-rot)))

(defn transition [state rot cz]
  (let [updated-rot (rotate (:dial state) rot)]
    {:zero-count (cz (:zero-count state) updated-rot)
     :dial (:updated-dial updated-rot)}))

(defn find-password [input trans] 
  (:zero-count
   (red trans
        (get-lines input)
        {:zero-count 0
         :dial 50})))

(defn part1 [input]
  (let [trans (fn [state rot] 
                (transition state rot count-zero-1))]
    (find-password input trans)))


(defn part2 [input]
  (let [trans (fn [state rot]
                (transition state rot count-zero-2))]
    (find-password input trans)))

