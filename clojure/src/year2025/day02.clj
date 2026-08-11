(ns year2025.day02 
  (:require
    [clojure.string :as str]))

(defn middle-index [id]
  (quot (count id) 2))

(defn first-half [id]
  (let [middle (middle-index id)]
    (subs id 0 middle)))

(defn second-half [id]
  (let [middle (middle-index id)]
    (subs id middle)))

(defn is-invalid? [id]
  (if (even? (count id)) 
    (= (first-half id) (second-half id))
    false))

(defn extract-invalid-ids [id-range]
  (reduce 
   (fn [acc id]
     (if (is-invalid? (str id))
       (conj acc id)
       acc))
   []
   (range
    (:first-id id-range)
    (inc (:last-id id-range)))
   ))

(defn sum [seq]
  (reduce + 0 seq))

(defn add-invalid-ids [ranges]
  (sum (flatten (map extract-invalid-ids ranges))))

(defn parse-range [line]
  (let [splitted (str/split (str/trim line) #"-")]
    {:first-id (Long/parseLong (first splitted))
     :last-id (Long/parseLong (last splitted))}))
(
 defn parse-ranges [input]
  (map parse-range
       (str/split input #",")))

(defn sliding-window [seq]
  (partition 2 1 seq))

(defn is-sorted? [seq]
  (every? 
   (fn [[a b]]
     (<= a b)) 
   (sliding-window seq)
   ))

(defn part-1 [input]
  (add-invalid-ids (parse-ranges input)))