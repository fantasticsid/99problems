(defn element-at [lst n]
  (if (= n 0)
    (first lst)
    (element-at (rest lst) (- n 1))))
