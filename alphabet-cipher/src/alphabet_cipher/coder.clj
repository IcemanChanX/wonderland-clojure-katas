(ns alphabet-cipher.coder)

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn getIndex [letter]
  (.indexOf alphabet (str letter))
  )

(defn convertToIndices [letterSequence]
  (map getIndex letterSequence)
  )

(defn lookupLetter [index]
  (str (nth (cycle alphabet) index))
  )

(defn encode [keyword message]
  (apply str
         (map lookupLetter
              (map +
                   (convertToIndices (take (count (seq message)) (cycle (seq keyword))))
                    (convertToIndices (seq message))
              )
          )
     )
  )

(defn decode [keyword message]
  (apply str
       (map lookupLetter
            (map (fn [x] (+ 26 x))
                 (map -
                      (convertToIndices (seq message))
                      (convertToIndices (take (count (seq message)) (cycle (seq keyword))))
                    )
              )
        )
    )
  )

(defn decipher [cypher message]
  (def a (atom 1))

  (def decyphered (apply str
         (map lookupLetter
              (map (fn [x] (+ 26 x))
                   (map -
                        (convertToIndices (seq cypher))
                        (convertToIndices (seq message))
                        )
                   )
              )
         )
    )

  (while (= false (= (take (count (seq decyphered)) (cycle (take @a decyphered))) (seq decyphered)))
    (do (take a decyphered) (swap! a inc))
    )
  (apply str (take @a decyphered))
  )

