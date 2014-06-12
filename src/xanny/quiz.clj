(ns xanny.quiz
  (:require [xanny.texts :as t]
            [xanny.utilities :refer :all]
            [clojure.string :as s]
            [clojure.pprint :refer :all]))

;create a latin dictionary quiz. it'll supply one true definiton, and 3 other random ones. It'll also tell you what words match those false answers 

;these are the [XXXXX] codes: AGE, AREA, GEO, FREQ, SOURCE
;file://localhost/Users/PCEye/Downloads/words-1.97Ed/wordsdoc.htm#Dictionary Codes
(defn latin-dict []
  (with-open [rdr (clojure.java.io/reader  "text-files/latin-dict.txt" :encoding "UTF-8")]
    (loop [seq (t/insert-line-if (line-seq rdr) "" #".+\[.+\]" :above)
           map {}
           entry []]
      (let [line (first seq)]
        ;(pprint entry)
        (cond (empty? seq)
              map
              (and (empty? line) (not (empty? entry)))
              (recur (rest seq) (conj map entry) [])
              (and (empty? entry) (not (empty? line)))
              (recur (rest seq) map [(first-word line) {:forms (rest-words line) :entry nil}])
              (empty? line)
              (recur (rest seq) map entry)
              :else (recur (rest seq) map 
                           [(first entry) (assoc (second entry) :entry
                                            (s/join " " [(:entry (second entry)) line]))]))))))

(def dict (latin-dict))


;have a different vocab lists for verbs
(def vocab {:nouns (s/split "agricola puella nihil fama forma fortuna ira nauta patria pecunia philosphia poena poeta porta puella rosa sententia vita ager amicus femina fillia fillius numerus populus puer sapientia vir avarus pauci basium bellum consillium cura donum exitium magister mora oculus officium otium periculum remedium  Romanus " #"\ ")
            :verbs (s/split "cogito amo debeo do erro laudo moneo salveo servo conservo terreo valeo habeo satio video voco iuvo sum" #"\ ")
            :adjectives (s/split "pulcher non antiquus magnus multus tuus meus"#"\ ")
            :adverbs (s/split "saepe semper bonus humanus bellus malus parvus stultus verus hodie" #"\ ")
            :other (s/split "si" #"\ ")})

(defn all-vocab []
  (map concat (vals vocab)))

;keep a list of words chosen, so the list of words decrements each time. keep a map of correct and wrong guesses, then let you replay with the words you got wrong. 
;save attempts to a file, and access the most recent entry, or overwrite it each time, or overwrite the entry for each set, so it separates noun attempts from verb ones. 
(defn vocab-quiz [words]
  (println "Welcome to the vocab quiz!")
  (loop [words-left words
         wrong-answers []]
    (if (empty? words-left)
      (if-not (empty? wrong-answers)
        (println wrong-answers "\nBeginning quiz with missed words" (vocab-quiz wrong-answers)) ;not working
        wrong-answers) 
      (let [word (rand-nth words-left)
            pick (dict word)]
        (println "The word is:" word "; it's forms being" (:forms pick))
        (println "Do you know the meaning? ") ;put this after the entry to confirm if you're right.
        (let [correct? (re-matches #"(?i)y|yes" (read-line))]
          (println "ENTRY:" (:entry pick))
          (println "Continue?" "\n")
          (if-not (re-matches #"(?i)n|no" (read-line))
            (recur (remove #{word} words-left)
                   (if correct?
                     wrong-answers
                     (conj wrong-answers word)))
            (if correct?
              wrong-answers
              (conj wrong-answers word))))))))
;use rand-nth for the quiz

;put the vocab words in certain forms

(def pronunciation {:consonants {} :vowels {} :dipthongs {}})

(def cases {:nom {:sg {1 {:fem "-a" :masc "-a"} 
                       2 {:masc "-us" :neut "-um"}} 
                  :pl {1 {:fem "-ae" :masc "-ae"} 
                       2 {:masc "-i" :neut "-a"}}}

            :gen {:sg {1 {:fem "-ae" :masc "-ae"} 
                       2 {:masc "-i" :neut "-i"}} 
                  :pl {1 {:fem "-arum" :masc "-arum"} 
                       2 {:masc "-orum" :neut "-orum"}}} 

            :dat {:sg {1 {:fem "-ae" :masc "-ae"} 
                       2 {:masc "-o" :neut "-o"}}
                  :pl {1 {:fem "-is" :masc "-is"} 
                       2 {:masc "-is" :neut "-is"}}} 

            :acc {:sg {1 {:fem "-am" :masc "-am"} 
                       2 {:masc "-um" :neut "-um"}}
                  :pl {1 {:fem "-as " :masc "-as"} 
                       2 {:masc "-os" :neut "-a"}}} 

            :abl {:sg {1 {:fem "-a" :masc "-a"} 
                       2 {:masc "-o" :neut "-o"}}
                  :pl {1 {:fem "-is" :masc "-is"} 
                       2 {:masc "-is" :neut "-is"}}} 

            :voc {:sg {1 {:fem "-a" :masc "-a"}
                       2 {:masc "-e" :neut "-um"}}
                  :pl {1 {:fem "-ae" :masc "-ae"}
                       2 {:masc "-i" :neut "-a"}}}})

(def vowels ["a" "e" "i" "o" "u"])

;perfect has its own endings. 
(def endings {:active {:sg {1 ["-o" "-m"] 2 "-s" 3 "-t"} :pl {1 "-mus" 2 "-tis" 3 "-nt"}}
              :passive {}})

(def conjugations {1 {:ending "-are" :stem "-a-"}
                   2 {:ending "-ere" :stem "-e-"}})

(def moods {:imperative "-te"})

(defn make-endings [endings]
  {:sg  (zipmap [1 2 3] (subvec endings 0 3))
   :pl (zipmap [1 2 3] (subvec endings 3 6))})

(def tenses {:present (make-endings ["-o" "-s" "-t" "-mus" "-tis" "-nt"]) 
             :future {1 2} ;show for each conjugation
             :imperfect {1 2}})

;if the ending is a vowel then replace the stem with it? 

(defn case-quiz [nth-decl] 
  (let [pick (case (rand-int 4)
               0 :nom
               1 :gen
               2 :dat
               3 :acc
               4 :abl
               5 :voc)
        number (case (rand-int 2)
                 0 :sg
                 1 :pl)
        decl (inc  (rand-int nth-decl))
        gender (rand-nth (keys (((pick cases) number) decl)))
        answer (gender (((pick cases) number) decl))]
    (println decl "declension |" pick "|" number  "|" gender)
    (println "Responsum tua: ")
    (let [input (read-line)]
      (if (= input answer)
        (println input "est corectio!")
        (println "Wrong! Answer is" answer)))))

(defn quiz-loop [nth-decl]
  (loop []
    (case-quiz nth-decl)
    (println "Procedo?")
    (if (= (read-line) "y")
      (recur)
      (println "Vale"))))

(defn print-decl [decl]
  (doseq [[case sg-pl] cases]
    (pprint case)
    (doseq [[num d] sg-pl]
      (pprint (d decl)))))

(defn get-declension [])


