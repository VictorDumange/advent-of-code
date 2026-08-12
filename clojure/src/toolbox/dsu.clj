(ns toolbox.dsu)


(defn make-dsu []
  {:parent {}
   :size {}})


(defn make-set [dsu elem]
  {:parent (assoc (:parent dsu) elem elem)
   :size (assoc (:size dsu) elem 1)})


(defn- get-parent [dsu elem]
  (get (:parent dsu) elem))


(defn root? [dsu elem]
  (= (get-parent dsu elem) elem))


;; Path compression.
;; Returns [root updated-dsu].
(defn find-set [dsu elem]
  (if (root? dsu elem)
    [elem dsu]
    (let [[root dsu] (find-set dsu (get-parent dsu elem))]
      [root
       {:size (:size dsu)
        :parent (assoc (:parent dsu) elem root)}])))


(defn get-size [dsu root]
  (get (:size dsu) root))


(defn groups [dsu]
  (keys (:size dsu)))


;; Attach the smaller tree to the bigger tree.
(defn- union-roots [dsu root1 root2]
  (let [size1 (get-size dsu root1)
        size2 (get-size dsu root2)]
    (if (> size1 size2)
      (union-roots dsu root2 root1)
      {:parent (assoc (:parent dsu) root1 root2)
       :size (-> (:size dsu)
                 (dissoc root1)
                 (assoc root2 (+ size1 size2)))})))


;; Connect the groups containing elem1 and elem2.
(defn union [dsu elem1 elem2]
  (let [[root1 dsu] (find-set dsu elem1)
        [root2 dsu] (find-set dsu elem2)]
    (if (= root1 root2)
      dsu
      (union-roots dsu root1 root2))))

(defn nb-groups [dsu]
  (count (:size dsu)))