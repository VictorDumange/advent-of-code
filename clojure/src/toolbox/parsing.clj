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