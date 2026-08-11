```clojure
(ns toolbox.intervals-test
  (:require [clojure.test :refer [deftest is]]
            [toolbox.intervals :as intervals]))


(deftest merge-both-test
  (is (= {:begin 1 :end 10}
         (intervals/merge-both
          {:begin 1 :end 5}
          {:begin 3 :end 10})))

  (is (= {:begin 1 :end 10}
         (intervals/merge-both
          {:begin 3 :end 10}
          {:begin 1 :end 5})))

  (is (= {:begin 1 :end 5}
         (intervals/merge-both
          {:begin 1 :end 5}
          {:begin 2 :end 4}))))


(deftest overlap-test
  ;; Partial overlap
  (is (true?
       (intervals/overlap?
        {:begin 1 :end 5}
        {:begin 3 :end 8})))

  ;; Reverse order
  (is (true?
       (intervals/overlap?
        {:begin 3 :end 8}
        {:begin 1 :end 5})))

  ;; One interval completely contains the other
  (is (true?
       (intervals/overlap?
        {:begin 1 :end 10}
        {:begin 3 :end 5})))

  ;; The intervals touch at their boundary
  (is (true?
       (intervals/overlap?
        {:begin 1 :end 5}
        {:begin 5 :end 10})))

  ;; Completely separate
  (is (false?
       (intervals/overlap?
        {:begin 1 :end 5}
        {:begin 6 :end 10})))

  ;; Separate in the other direction
  (is (false?
       (intervals/overlap?
        {:begin 6 :end 10}
        {:begin 1 :end 5}))))

(deftest merge-test
  ;; Already sorted and disjoint
  (is (= [{:begin 1 :end 3}
          {:begin 5 :end 8}]
         (intervals/merge
          [{:begin 1 :end 3}
           {:begin 5 :end 8}])))

  ;; Overlapping intervals are merged
  (is (= [{:begin 1 :end 8}]
         (intervals/merge
          [{:begin 1 :end 5}
           {:begin 3 :end 8}])))

  ;; Multiple overlapping intervals
  (is (= [{:begin 1 :end 10}]
         (intervals/merge
          [{:begin 1 :end 5}
           {:begin 3 :end 7}
           {:begin 6 :end 10}])))

  ;; One interval contains another
  (is (= [{:begin 1 :end 10}]
         (intervals/merge
          [{:begin 1 :end 10}
           {:begin 3 :end 5}])))

  ;; Input does not need to be sorted
  (is (= [{:begin 1 :end 8}
          {:begin 10 :end 15}]
         (intervals/merge
          [{:begin 10 :end 15}
           {:begin 3 :end 8}
           {:begin 1 :end 5}])))

  ;; Intervals touching at the boundary are merged
  (is (= [{:begin 1 :end 10}]
         (intervals/merge
          [{:begin 1 :end 5}
           {:begin 5 :end 10}])))

  ;; Several independent groups
  (is (= [{:begin 1 :end 8}
          {:begin 10 :end 15}
          {:begin 20 :end 25}]
         (intervals/merge
          [{:begin 20 :end 25}
           {:begin 10 :end 12}
           {:begin 1 :end 5}
           {:begin 3 :end 8}
           {:begin 12 :end 15}]))))
