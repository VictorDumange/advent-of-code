(ns toolbox.table-test
  (:require [clojure.test :refer [deftest is]]
            [toolbox.table :as table]))


(deftest at-test
  (let [t [[1 2 3]
           [4 5 6]]]
    (is (= 1 (table/at t 0 0)))
    (is (= 6 (table/at t 1 2)))))


(deftest nb-cols-test
  (is (= 3
         (table/nb-cols
          [[1 2 3]
           [4 5 6]]))))


(deftest nb-rows-test
  (is (= 2
         (table/nb-rows
          [[1 2 3]
           [4 5 6]]))))


(deftest nth-column-test
  (let [t [[1 2 3]
           [4 5 6]
           [7 8 9]]]
    (is (= [1 4 7]
           (table/nth-column t 0)))

    (is (= [2 5 8]
           (table/nth-column t 1)))

    (is (= [3 6 9]
           (table/nth-column t 2)))))


(deftest cols-test
  (is (= [[1 4 7]
          [2 5 8]
          [3 6 9]]
         (table/cols
          [[1 2 3]
           [4 5 6]
           [7 8 9]])))

  ;; Non-square table
  (is (= [[1 4]
          [2 5]
          [3 6]]
         (table/cols
          [[1 2 3]
           [4 5 6]]))))

(deftest map-table-test
  (is (= [[2 3 4]
          [5 6 7]]
         (table/map-table inc
                          [[1 2 3]
                           [4 5 6]])))

  (is (= [[2 4 6]
          [8 10 12]]
         (table/map-table #(* 2 %)
                          [[1 2 3]
                           [4 5 6]]))))


(deftest parse-table-test
  (is (= [["1" "2" "3"]
          ["4" "5" "6"]]
         (table/parse-table
          "1 2 3
4 5 6")))

  ;; Multiple spaces between elements
  (is (= [["1" "2" "3"]
          ["4" "5" "6"]]
         (table/parse-table
          "  1   2     3
4  5  6"))))

(deftest rotate-test
  (is (= [[6 2 3]
          [4 3 1]
          [4]]
       (table/rotate
        [[6 4]
         [2 3]
         [3 1 4]]))))