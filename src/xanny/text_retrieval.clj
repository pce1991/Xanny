(ns xanny.text-retrieval
  [:require [xanny.text_processing :refer :all]])

;;; build a concordance for the bible. Get a corpora of every word, and for each one, record the index it occured in. Then add these so that each verse will point to every other verse which has the same word. Toss out basic things like prepositions, a, the, it, etcetera. Expand to work with a thesaurus so it captures more meaning. 


;;; change fetch so it takes a vector instead of map, you can just supply _ to indicate you don't care about that entry. 
(defn fetch
  "Takes a map `m` keyed by vectors and a map `coords`
  keyed by indices into those vectors.

  Returns a submap of `m` that includes all on only the keys
  which agree with `coords` on the indices mentioned in `coords`."
  [m coords]
  (into (sorted-map-by compare-index)
    (for [[k :as e] m
          :when (every? #(= (nth k %) (get coords %))
                  (keys coords))]
      e)))

