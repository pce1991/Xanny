(ns xanny.intertwinularity
  (:require [xanny.nlp :refer :all]
            [xanny.utilities :as util]
            [clojure.string :as s]))                  ;sorry Nelso, there should be no "g," it's to get the twin. 

;;; LOOM of Nexealist 

;;; RUN THIS ON THE BIBLE!q

;;; use Incanter

;;; the corpus is just an indexed collection of passages. 
;;; How to handle the complex indices I'm using for texts, where the text has two keys to reach it, and within it
;;; there are five indices. I could just map the indices created here to the appropiate ones in the text, but that
;;; sounds hacky. Oh! I can just modify corpus to be map where the index is either supplied or just increased. 
(def corpus ["The quick brown fox lept and ran."
             "The quick black dog ran down the gate."
             "I ran home quickly."
             "You can't run from your problems."
             "Run as you might."])

;;; if it's given a map like what's being used elsewhere, then it'll grab pairs, it it isn't a map but a sequence,
;;; then it'll put it into a numerically indexed map. This is pretty silly now that I think about it. What I need
;;; is something that takes maps of texts, and can create links between them properly, but also creates links
;;; within one text. I think then indexes in map-text should be [author title [0 1 2 3 4]] but that's kinda clunky
;;; it does allow me to treat the english corpus as just sentences and lines and pull authors and titles out of it.
;;; hmmmmm
(defn gen-corpus [text-map]
  )

;; {"The quick brown fox." {"quick" {"The quick black dog." 1}}
;;  "The quick black dog." {"quick" {"The quick brow fox." 1}}}
;; ;;; this might replicate data too much, I don't want a copy of the second passage, just a link to it. 
;; ;;; this means passages should be pairs of [key passage] taken from a vector. 
;; ["The quick brown fox." "The quick black dog."]
;; ;;; this would be passed in as:
;; [0 "The quick brown fox."] [1 "The quick black dog."]
;; ;;; and return
;; {"The quick brown fox." {"quick" {1 1}}
;;  "The quick black dog." {"quick" {0 1}}}
;; ;;; which would get merged with a global map which might have strings as keys, or maybe just indexed numbers
;; ;;; should the map also record it's own position in the text like below? Or maybe just a map {position {link pos}}
;; {"The quick brown fox." {"quick" [{0 1} {1 1}]}
;;  "The quick black dog." {"quick" [{1 1} {0 1}]}}
;;; I don't think so. 

;;; in addition to this write maps to have two types of links: the linguistic, the referential, the personal
;;; and the marginal (notes). 
;;; I should abstract the basic idea into a function which adds a link to two documents.
;;; it takes a thing to add an index to add to it, then takes the first thing and links it in the second. 
;;; Make this work for all types of links in a general way! 
(defn add-link [passage addition]
  "Takes a map of the passage and all its links, and adds a new link designated {type-key {index position}}")

;;; how to handle quoted notes? Does a quoted paragraph link each of its sentences? No, it should find the largest
;;; sequence that appears in another place. That's exhausting tho, find a better way. Going by sentence isnt so bad
;;; but misses things like "and then he soid that... the film was good." which should be an acceptable way to quote
;;; (rather to ammend a full quote when displayed). 


;;; contains? works on sets, so you turn the coll2 into a set, and use the items in coll1 to lookup. 
;;; this won't be very general, will assume that both collections are. Use some?
(defn share? [coll1 coll2]
  "Checks to see if coll1 and coll2 share an item. Returns true if anything in coll2 is found in coll1."
  (loop [items coll2]                   
    (if (empty? items)
      false
      (if (some #{(first items)} coll1)
        true
        (recur (rest items))))))

;;; if the list of lemmas share a single pair then they should be merged. So for each lemma list, you loop thru it and check each item to see if its contained in the other, at the first hit combine them. 
(defn shared-lemmas [lemma lemmas]
  "Returns the pair in lemmas that it shares with lemma."
  (loop [find-in lemmas]  ;make into a function!
    (if (empty? find-in)
      nil
      (if (share? (val lemma) (val (first find-in)))
        (first find-in)
        (recur (into {} (rest find-in)))))))


;;; instead of lemmas just compare words and see how similar they are using wordnet. Write something to guess similarity by summing up the similarities of each word to every other word in a passage, then summing the results. 

;;; this doesn't entwine things adequetly enough. Example: quck and quickly should be linked, but they aren't since a lemma isnt shared. Maybe there should be criteria beyond lemmatization, like word-similarity!
;;; modify this to put the index in the work map? Make it add these under linguistic links, not as the only kind
(defn entwine [pair1 pair2]
  "Takes two pairs of index and passage, then returns a map with those passages as keys mapped to a map of lemmas keys mapped to their position in another text, and the index of that text."
  (let [index1 (pair1 0)
        index2 (pair2 0)
        passage1 (pair1 1)
        passage2 (pair2 1)
        lemmas1 (lemmatize passage1) 
        lemmas2 (lemmatize passage2)]
    (loop [to-entwine lemmas1
           p1 {passage1 {}}             
           p2 {passage2 {}}]
      (if (empty? to-entwine)
        (merge p1 p2) ;use index as key instead of passage! update braid and weave then!
        (let [lemma (first to-entwine)
              pos-in-1 (util/word-position (key lemma) (s/lower-case passage1))
              shared (shared-lemmas lemma lemmas2) 
              pos-in-2 (if-not (nil? shared) 
                         (util/word-position (key shared) (s/lower-case passage2)) nil)]
          (if-not (nil? shared) 
            (recur (into {} (rest to-entwine))
                   (update-in p1 [passage1] conj {[(key lemma) pos-in-1] {index2 pos-in-2}})
                   (update-in p2 [passage2] conj {[(key shared) pos-in-2] {index1 pos-in-1}}))
            (recur (into {} (rest to-entwine)) p1 p2)))))))


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
(defn weave [corpus]
  (loop [map {}
         index 0]
    (if (= index (count corpus))
      map
      (recur (merge-with #(merge-with  merge %1 %2) map (braid [index (get corpus index)] corpus))
             (inc index)))))            ;seems right, but check on larger sets, use tests. 

;;; scale it up to concat consecutive matche lemmas.
;;; for lager corpi it might be best to list a minimum number of lemma matches to be considered based on the size of the passage; a smaller one might only require 3 hits to be linked to something, but a lagre one might require 10 
;;; Part of scaling it up will be giving a substring and range a link either to a single point or a text and range

;;; Bookmarks and highlighting
;;; a bookmark marks a point of reentry. A highlight is a noteable segment with a start and end. 

;;; a note is just a pointer containing an index or range (segment) keyed to value which is the note. The problem then is that notes do not exist outside of the text that points to them. Perhaps instead each user should have a collection which is notes, and each of those notes points to a text and place in it. Adding a note causes the noted text to contain a pointer to the note as well. 
