(ns xanny.nlp
     (:use [opennlp.nlp]
           [opennlp.treebank]
           [clj-wordnet.core]
           [xanny.utilities]
           )
     (:require [clojure.string :as s]))
;==========================================================================================
;==========================================================================================
;OPENNLP
;==========================================================================================
;==========================================================================================

(def get-sentences (make-sentence-detector "NLP/models/en-sent.bin"))
(def tokenize (make-tokenizer "NLP/models/en-token.bin"))
(def detokenize (make-detokenizer "NLP/models/english-detokenizer.xml"))
(def pos-tag (make-pos-tagger "NLP/models/en-pos-maxent.bin"))
(def name-find (make-name-finder "NLP/models/namefind/en-ner-person.bin")) 
;these kill refresh :(
;(def chunker (make-treebank-chunker "NLP/models/en-chunker.bin"))
;(def treebank-parser (make-treebank-parser "NLP/models/en-parser-chunking.bin"))

;write tokenizer that separates quotes from beginning of words, also ! and --

;so make-tree on treebank-parser to get something thats data rather than a string; its so
;GACK though. I'd like something a lot more useful than :chunk and :tag, much better to lookup
;by :PP :NNP etcetera. or am I misguided? 

;==========================================================================================
;==========================================================================================
;WORDNET
;==========================================================================================
;==========================================================================================
(def wordnet (make-dictionary "NLP/WordNet-3.0"))


;==========================================================================================
;==========================================================================================
;MARKOV GENERATION
;==========================================================================================
;==========================================================================================

;for trigrams i want how many times each word follows another, but base probabilities on
;the two previonus words barks | the, dog
;will my map be different? it could still pair words, but then how would i know the 
;probability two words... if start= the then there's a %50 dog %50 cat, from there the next
;dog it might go %50 run %50 bite, cat %25 run %25 claw %50 jump then the prob of run is given by 
;the two previous words. 
;this is what i want: x might follow y %50 of the time, and z follows x %25 of the time, but 
;%10 if x follows y, i need word pairs for that, or how do i represent it? 

(def names ["Hector" "Odysseus" "Achilles"])

(defn merge-gram [map addition]
  "The addition will be a map of {new-letter {following-letter count}} and will be merged onto the map by conjing the new-letter with its contents in the map, and merging the following letter with the count of it in new-letter in the map."
  (merge-with (fn [letters letter] (conj letters (merge-with + letters letter))) 
              map addition))

(defn n-gram [n coll]
  (patition n 1 coll))

;go through each letter of every word, whenever a letter is encountered that isnt in the map, add it to the map, you then keep track of the current letter, and see what follows it. So you add H to the map, setting current letter to H, then an e is encountered so you add it to H in map while incrementing the count if its already there, then set the current letter to E to see what follows it. Don't set the current letter if its the last char in a word. 
;use starts and stops
; use the given n-gram/partition function to patition the words first. Doing that 
(defn make-n-gram [words-vector]
  (loop [words (map s/upper-case  words-vector)
             map {:START {} :STOP {}}]
    (if (empty? words)
      map
      (recur (rest words) 
             (loop [characters (string-seq (first words)) ;(map s/capitalize  (map str  (string-seq (first words))))
                    started? false
                    m map]
               (if (= (count characters) 1)
                 (merge-gram m {:STOP {(first characters)  1}})
                 (if started? ;if it isnt started then add the first letter to start, but dont move past it yet. 
                   (recur (rest characters) true (merge-gram m {(first characters) {(second characters) 1}}))
                   (recur characters true (merge-gram m {:START {(first characters) 1}})))))))))

;;; generate-names. the liklihood of each letter is dependent on its frequency, given the frequency of the last word, which I think is the probability of the first and second / the prob of first. 
