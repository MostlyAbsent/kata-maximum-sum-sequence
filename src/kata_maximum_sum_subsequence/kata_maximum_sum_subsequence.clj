(ns kata-maximum-sum-subsequence.kata-maximum-sum-subsequence)

(defn all-subvecs [v]
  (loop [start (reverse (range (.length v)))
        end (reverse (range (.length v)))
        result #{v}]
   (cond
     (empty? end)
     (recur (rest start) (reverse (range (.length v))) result)
     (empty? start)
     result
     (<= (first start)
         (first end)) (recur start
                             (rest end)
                             (conj result (subvec v
                                                  (first start)
                                                  (first end))))
     :else
     (recur (rest start) (reverse (range (.length v))) result))))

(defn all-pos? [v]
  (every? true? (map pos? v)))

(defn max-sequence [xs]
  (let [v (vec xs)]
    (cond
      (empty? v)
      0
      (all-pos? v)
      (reduce + v)
      :else
      (->>
       (all-subvecs v)
       (map (fn [x] {:sum (reduce + x) :vec x}))
       (sort-by :sum >)
       first
       :sum))))
