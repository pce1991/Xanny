(ns xanny.utilities
  (:require [clojure.string :as string]))

;==========================================================================================
;==========================================================================================
;UTILITIES
;==========================================================================================
;==========================================================================================

;=============================================
;COLLECTION
;=============================================
(defn element-count [coll element]
  (count (filter #(= element %) coll)))

(defn sublist 
  ([list start]  (seq (subvec (into [] list) start)))
  ([list start stop] (seq (subvec (into [] list) start stop))))

;=============================================
;STRING TRIMMING
;=============================================

(defn punctuation? [])

;doesnt work for non-english chars
(defn letter? [char]
  (re-matches #"[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]" 
              char))

(defn pattern? [pat]
  "checks to see if an element is a regex pattern." 
  (= java.util.regex.Pattern (type pat)))

(defn string-seq [str]
  "returns a sequence of all the characters in the string."
  (seq (char-array str)))

;change to newline?
(defn newline? [str]
  (if (= (count str) 1)
    (= (first (string-seq str)) \return)
    false))

;change this to yelling? 
(defn uppercase? [str]
  (= str (string/upper-case str)))

(defn trim-verse [str]
  (string/triml (string/replace str #"[0123456789]" "")))

;include more oddities as they're encountered.
(defn trim-oddities [str]
  (string/trimr (string/triml (string/replace str #"[\_\[\]\#]" "")))) ;not sure trimming right is safe. 

;include something to remove unwanted punctuation like # signs. 
(defn cleanup-string [str]
  (trim-oddities (trim-verse (string/trim-newline str))))

;strip string to remove every non-letter. char array, filter letter

(defn indented? [str]
  (if (>= (count str) 4)
    (= (subvec (into [] (seq str)) 0 4) 
       (into [] (seq "    ")))
    false))

;justified is just 1 > than indented
(defn justified? [line]
  (if (>= (count str) 5)
    (= (subvec (into [] (seq str)) 0 5) 
       (into [] (seq "     ")))
    false))
  
