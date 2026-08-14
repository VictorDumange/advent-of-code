(ns toolbox.graph)

(defrecord Node [value previous])

(defn- enqueue-all [queue nodes]
  (reduce conj queue nodes))

(defn- next-nodes [context node neighbors seen]
  (map
   (fn [value]
     (Node. value node))
   (remove
    (fn [value]
      (contains? seen value))
    (neighbors context (:value node)))))

(defn- mark-seen [seen nodes]
  (into seen (map :value nodes)))

(defn- bfs-iter [context queue seen neighbors goal?]
  (if (empty? queue)
    nil
    (let [node (peek queue)]
      (if (goal? context (:value node))
        node
        (let [queue (pop queue)
              nodes (next-nodes context node neighbors seen)]
          (bfs-iter
           context
           (enqueue-all queue nodes)
           (mark-seen seen nodes)
           neighbors
           goal?))))))

(defn bfs [context initial-value neighbors goal?]
  (bfs-iter
   context
   (conj clojure.lang.PersistentQueue/EMPTY
         (Node. initial-value nil))
   #{initial-value}
   neighbors
   goal?))

(defn path [node]
  (if (nil? node)
    '()
    (conj (path (:previous node))
          (:value node))))

(defn path-length [path]
  (dec (count path)))