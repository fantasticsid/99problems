(defn compress [lst]
  (if (< (count lst) 2)
    lst
    (if (= (first lst)
           (first (rest lst)))
      (compress (rest lst))
      (cons (first lst) (compress (rest lst))))))
