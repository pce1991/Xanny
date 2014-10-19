(ns xanny.intertwinularity
  (:require [xanny.nlp :refer :all]
            [xanny.utilities :as util]))                  ;sorry Nelso, there should be no "g"

(def corpus ["The quick brown fox lept and ran."
             "The quick black dog ran down the gate."
             "I ran home quickly."
             "You can't run from your problems."
             "Run as you might."])

;;; get some dictionary stuff working so I can just have a corpus of words organized by type, and just grad articles here; use GNU CIDE
;;; since wordnet doesn't even have entries for these it might not even be needed. 
(def muted-words {:articles 
                  :pronouns
                  :prepositions
                  :conjunctions 
                  :determiners
                  :particles
                  })


{"The quick brown fox." {"quick" {"The quick black dog." 1}}
 "The quick black dog." {"quick" {"The quick brow fox." 1}}}
;;; this might replicate data too much, I don't want a copy of the second passage, just a link to it. 
;;; this means passages should be pairs of [key passage] taken from a vector. 
["The quick brown fox." "The quick black dog."]
;;; this would be passed in as:
[0 "The quick brown fox."] [1 "The quick black dog."]
;;; and return
{"The quick brown fox." {"quick" {1 1}}
 "The quick black dog." {"quick" {0 1}}}
;;; which would get merged with a global map which might have strings as keys, or maybe just indexed numbers
;;; should the map also record it's own position in the text like below? Or maybe just a map {position {link pos}}
{"The quick brown fox." {"quick" [{0 1} {1 1}]}
 "The quick black dog." {"quick" [{1 1} {0 1}]}}

;;; creates links between two passages. A link is always recorded twice! 
(defn entwine [pair1 pair2]
  "Takes two pairs of index and passage, then returns a map with those passages as keys mapped to a map of lemmas keys mapped to their position in another text, and the index of that text."
  (let [index1 (pair1 0)
        index2 (pair2 0)
        passage1 (pair1 1)
        passage2 (pair2 1)
        lemmas1 (lemmatize passage1) 
        lemmas2 (lemmatize passage2)
        shared (into {} (clojure.set/intersection (into #{} lemmas1) (into #{} lemmas2)))
        ;; there's an logic error here! Some lemmas should be counted the same, but are different vectors
        ;; (["run" :noun] ["run" :verb]) vs (["run" :verb])
        ;; I want them to be counted as shared if they have a matching pair anywhere, but are they then
        ;; concacted? 
        ]

    ;; once we have the shared the lemmas, we loop thru them, for each one recording it's position in both passages, then take the lemma as a key mapped to {opposite-text-index position}
    (loop [p1 {passage1 {}}
           p2 {passage2 {}}
           to-find shared]
      (if (empty? to-find)
        (merge p1 p2)
        (let [lemma (first to-find)
              pos-in-1 (util/word-position (key lemma) passage1)
              pos-in-2 (util/word-position (key lemma) passage2)]
          (recur (update-in p1 [passage1] conj {(key  lemma) {index2 pos-in-2}})
                 (update-in p2 [passage2] conj {(key  lemma) {index1 pos-in-1}})
                 (into {} (rest to-find))))))))


;;; Creates a connection between a given passage and the corpus. 
(defn braid [passage corpus]
  "Entwines a passage to everything in the corpus, merging the lemma maps together to include every passage and position that a lemma points to. "
  ;checks to see if the passage is contained in the corpus and removes it if so. 
  (loop [map {}
         index (dec  (count corpus))]
    (if (>= index 0)
      (if-not (= passage [index (get corpus index)])
        (recur  (merge-with #(merge-with merge %1 %2) map (entwine passage [index (get corpus index)]))
                (dec index))
        (recur map (dec index)))        ;don't entwine a text to itself
       map)))

;;; ties every thing in a coruus to everything else
(defn weave [corpus])
