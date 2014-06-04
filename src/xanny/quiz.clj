(ns xanny.quiz
  (:require [xanny.texts :as t]
            [clojure.string :as s]
            [clojure.pprint :refer :all]))

;create a latin dictionary quiz. it'll supply one true definiton, and 3 other random ones. It'll also tell you what words match those false answers 

(def l (t/map-dictionary-latin))

(def vocab (s/split "agricola " #"\ "))
;use rand-nth for the quiz

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

(def vowels [a e i o u])

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
             :imperfect})

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


