(ns xanny.generator
  (:require [xanny.nlp :as nlp]
            [clojure.string :as s]))

;try a Norvig's technique where I conform to some rules and patterns. It'll be more formulaic, but that isnt necessarily a problem for the domain. 

;create a list of names, a list of places, a list of roles, and actions. 

;grab these from a corpus. make an n-gram of how often letters follow each other to create new names. 
(def names [])

;use a markov sequence of letters to create new names. 
(defn new-name [])

;create a variation on a phrase to use in repetition. 

;generate fables from a list of morals. Expand it to create more awbiguity in the lesson. 
