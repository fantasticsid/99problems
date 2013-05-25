(defn myReverse [lst]
  (if (empty? lst)
    []
    (conj (myReverse (rest lst)) (first lst))))
