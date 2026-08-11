(ns toolbox.intervals)

;; merge two intervals
;; unsafe : overlap check is not performed
(defn merge-both [interval-1 interval-2]
  {:begin (min (:begin interval-1) (:begin interval-2))
   :end (max (:end interval-1) (:end interval-2))})

(defn left? [ref cand]
  (< (:end cand) (:begin ref)))

(defn right? [ref cand]
  (> (:begin cand) (:end ref)))

(defn overlap? [interval-1 interval-2]
  (and
   (not (left? interval-1 interval-2))
   (not (right? interval-1 interval-2))))

(defn- merge-sorted [intervals] 
  (reduce 
   (fn [merged cur-interval]
     (let [previous (peek merged)]
       (if (overlap? previous cur-interval)
         (conj (pop merged) (merge-both previous cur-interval))
         (conj merged cur-interval))))
   [(first intervals)]
   (rest intervals)
   ))

(defn merge [intervals]
  (merge-sorted
   (sort-by 
    (fn [i] (:begin i))
    intervals)))