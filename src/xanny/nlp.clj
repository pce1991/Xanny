(ns xanny.nlp
     (:use [opennlp.nlp]
           [opennlp.treebank]
           [clj-wordnet.core]
           [xanny.utilities]
           ))
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

