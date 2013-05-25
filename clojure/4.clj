(defn length [lst]
  (if (empty? lst)
    0
    (inc (length (rest lst)))))
