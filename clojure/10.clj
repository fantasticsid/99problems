(defn pack [lst]
  (if (empty? lst)
    [[]]
    (if (= (count lst) 1)
      [lst]
      (let [packtail (pack (rest lst))]
        (if (= (first lst) (first (rest lst)))
          (cons (cons (first lst)
                      (first packtail))
                (rest packtail))
          (cons [(first lst)] packtail))))))

(defn encode [lst]
  (let [grouped_list (pack lst)]
  (map #(list (count %) (first %)) grouped_list)))
