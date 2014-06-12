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

;doesnt work for non-english chars
(defn letter? [char]
  (re-matches #"[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]" 
              char))

(defn digit? [ch]
  (if (some #{ch} '(\1 \2 \3 \4 \5 \6 \7 \8 \9 \0))
    true
    false))

;do regex work on a char? or is the seq I'll get of length 1 strings.
(defn punctuation? [ch]
  (if (some #{ch} '(\ \.\,\'\\\;\:\!\?\/\+\=\-\{\}\[\]\(\)))
    true
    false))

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

(defn string-contains-int [str]
  "returns true if there's a number in the string"
  (some digit? (seq str)))

(defn integer-string? [str]
  "returns true if every char is a number; ignores punctuation"
  (and  (every?  (fn [ln] (or (digit? ln) (punctuation? ln))) (seq str))
        (string-contains-int str)))

;change this to yelling? 
(defn uppercase? [str]
  (if (and  (not  (empty? str)) (not (integer-string? str)))
    (= str (string/upper-case str))
    false))

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
    (= (subs str 0 4) "    ")
    false))

;consider this returning false if it's wore than two spaces over.
(defn notched? [str]
  (if (>= (count str) 2)
    (= (subs str 0 2) "  ")
    false))

(defn notched-n? [str n]
  (if (and  (>= (count str) n)
            (re-matches (re-pattern (str "\\ {" n ",}")) str))
    true
    false))
;include one to check if its just indented twolines, or maybe check if its totally left justified

;these are both TOO SLOW! use regex instead!

;justified is thrice indented?
(defn justified? [line]
  (if (>= (count line) 12)
    (= (subs line 0 12)  "            ")
    false))

(defn left-justified? [str]
  (if (not (empty? str))
    (not (= (subs str 0 1) " "))
    false))

(defn last-char [string]
  (last (seq string)))

(defn first-char [str]
  (first (seq str)))

(defn trim-punctuation [string]
  (loop [s (reverse (seq string))]
    (if (punctuation? (first s))
      (recur (rest s))
      (apply str  (reverse s)))))

(defn first-word [string]
  (string/replace (first (string/split string #"\ ")) #"," ""))

(defn rest-words [string]
  (string/join " " (rest (string/split string #"\ "))))

(defn end-puntuation? [ch]
  (if (some #{ch} '(\.\!\?))
    true
    false))

(defn sentence-end? [str]
  (if (end-puntuation? (last-char str))
    true
    false))

(defn capitalized? [string]
  (uppercase? (str (first-char string))))

(defn complete-sentence? [str]
  (and (capitalized? str) (sentence-end? str)))

;this is a prett version of normal split. It'll retain the char where the split occurs, giving it to the first segment, and will trim the spaces off of the second segment. 
(defn string-split [str split-at])

;also from contrib. 
(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.
  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

 
;from contrib
(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))


;doesnt retain front and end spaces right now... 
(defn cut-gaps [str]
  "Similar to triml, but it retains spaces at the beginning, removes expanses in between words."
  (let [s (string/split str #"\ ")
        l-spaces (if (empty? (first s)) 
                   (subvec s (first (positions empty? s)) (first (positions not-empty s)))
                   [])]
    (string/join " " (concat l-spaces (remove empty? s)))))


(defn re-matches-all [pats str]
  (every? (fn [pat] (re-matches pat str)) pats))

;returns the first match
(defn re-matches-some [pats str]
  (some (fn [pat] (re-matches pat str)) pats))

;returs t if it only matches one of the patterns provided. 
(defn re-matches-one [pats str])

(defn which-matches? [patterns str]
  "Returns the first match"
  (loop [pats patterns]
    (if (empty? pats)
      nil
      (if (re-matches (first pats) str)
        (first pats)
        (recur (rest pats))))))

;split can bu used to get the rest, but It'll fail if the pattern retains the .* stuff
;if the user doesnt suplly them as regex, but I do that here, then they're just strings and can be modified. 
;DUH, str does what I need. Problem solved, just split it where things like . .* + etcetera occur. 
(defn re-rest [pat str]
  "If there is a match it'll return everything that occurs after the supplied pattern. #ACT .* matched to ACT II will return II."
  
)

(defn occurences [element seq]
  (count  (filter #{element} seq)))
;=============================================
;FILES
;=============================================
;get the count of everything mathching a regex in, like how many lines end in a period.

