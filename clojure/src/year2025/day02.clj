(ns year2025.day02 
  (:require
    [clojure.string :as str]))

(defn parse-range [line]
  (let [splitted (str/split (str/trim line) #"-")]
    {:first-id (Long/parseLong (first splitted))
     :last-id (Long/parseLong (last splitted))}))

(defn parse-ranges [input]
  (map parse-range
       (str/split input #",")))

(defn middle-index [id]
  (quot (count id) 2))

(defn first-half [id]
  (let [middle (middle-index id)]
    (subs id 0 middle)))

(defn second-half [id]
  (let [middle (middle-index id)]
    (subs id middle)))

(defn repeated-twice? [id]
  (if (even? (count id)) 
    (= (first-half id) (second-half id))
    false))

(defn extract-invalid-ids [id-range id-invalid?]
  (reduce 
   (fn [acc id]
     (if (id-invalid? (str id))
       (conj acc id)
       acc))
   []
   (range
    (:first-id id-range)
    (inc (:last-id id-range)))))

(defn sum [seq]
  (reduce + 0 seq))

(defn add-invalid-ids [ranges id-invalid?]
  (sum 
   (flatten 
    (map 
     (fn [range] 
       (extract-invalid-ids range id-invalid?)) 
     ranges))))

(defn sliding-window [seq]
  (partition 2 1 seq))

(defn lps-iter [pattern length i lps]
  (if (= i (count pattern))
    lps
    (cond
      (= (nth pattern length) (nth pattern i))
      (recur pattern
             (inc length)
             (inc i)
             (conj lps (inc length)))

      (zero? length)
      (recur pattern
             0
             (inc i)
             (conj lps 0))

      :else
      (recur pattern
             (nth lps (dec length))
             i
             lps))))

;; longest prefix suffix table
(defn longest-prefix-suffix [pattern]
  (lps-iter pattern 0 1 [0]))

(defn period [input]
  (- 
   (count input)
   (last (longest-prefix-suffix input))))

(defn repetitive-pattern? [input]
  (let [p (period input)
        n (count input)]
    (and
     (< p n)
     (zero? (mod n p)))))

(defn part-1 [input]
  (add-invalid-ids (parse-ranges input) repeated-twice?))

(defn part-2 [input]
  (add-invalid-ids (parse-ranges input) repetitive-pattern?))


