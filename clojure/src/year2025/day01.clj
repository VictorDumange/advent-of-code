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

;; mod seems very high level
;; TODO : custom modulo
(defn rotate [dial rot]
  (mod 
   ((direction rot) dial (distance rot))
   100))

(defn transition [state rot]
  (let [updated-dial (rotate (:dial state) rot)]
     {:zero-count (if (= 0 updated-dial)
                    (inc (:zero-count state))
                    (:zero-count state))
      :dial updated-dial}))

(defn part1 [input]
  (:zero-count
   (red transition
        (get-lines input)
        {:zero-count 0
         :dial 50}))
  )