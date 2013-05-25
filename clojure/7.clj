(defn myFlatten [lst]
  (if (empty? lst)
    nil
    (if (coll? (first lst))
      (concat
       (myFlatten (first lst))
       (myFlatten (rest lst)))
      (cons (first lst)
            (myFlatten (rest lst))))))

(defn myFlatten2 [lst]
  (if (coll? lst)
    (mapcat myFlatten2 lst)
    [lst]))
