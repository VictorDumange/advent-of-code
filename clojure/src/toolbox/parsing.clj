(ns toolbox.parsing 
  (:require [clojure.string :as str]))

;; delimiter can be a string or a char
(defn split [input regex]
  (str/split input regex))

(defn parse-lines [input]
  (str/split-lines input))

(defn parse-int [input]
  (Integer/parseInt (str/trim input)))

(defn parse-long [input]
  (Long/parseLong (str/trim input)))

(defn digit [c]
  (Character/digit c 10))

(defn trim [line]
  (str/trim line))

(defn- reversed-digits-iter [nb output]
  (if (= 0 nb)
    output
    (recur (quot nb 10) (conj output (rem nb 10)))))

(defn digits [nb]
  (if (= 0 nb)
    [0]
    (reverse (reversed-digits-iter nb []))))