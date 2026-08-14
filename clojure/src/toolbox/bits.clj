(ns toolbox.bits)

(defn single-number [numbers]
  (reduce
   (fn [acc n] (bit-xor acc n))
   0
   numbers))

(defn at [num i]
  (bit-and num (bit-shift-left 1 i)))

(defn test-at [num i]
  (= 1 (at num i)))

(defn toggle [num i]
  (bit-xor num (bit-shift-left 1 i)))

(defn set-at [num i]
  (bit-xor num (bit-shift-left num i)))

(defn bits-number [num] 
  (if (= 0 num) 
    0
    (inc (bits-number (bit-shift-right num 1)))))