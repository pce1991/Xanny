(ns xanny.quiz
  (:require [xanny.texts :as t]
            [clojure.string :as s]))

;create a latin dictionary quiz. it'll supply one true definiton, and 3 other random ones. It'll also tell you what words match those false answers 

(def l (t/map-dictionary-latin))

(def vocab (s/split "agricola " #"\ "))
;use rand-nth for the quiz

(def cases {:nom {:sg {1 {:fem "-a" :masc "-a"} 2 {:masc "-us" :neut "-um"}} 
                  :pl {1 {:fem "-ae" :masc "-ae"} 2 {:masc "-i" :neut "-a"}}}
            :gen {:sg {1 {:fem "-ae" :masc "-ae"} 2 {:masc :neut}} :pl } :dat {:sg :pl} :acc {:sg :pl} :abl {:sg :pl} :voc {:sg :pl}})


(defn case-quiz [] 
  (let [pick (case (rand-int 4)
               0 :nom
               1 :gen
               2 :dat
               3 :acc
               4 :abl)
        number (case (rand-int 2)
                 0 :sg
                 1 :pl)
        decl (inc  (rand-int 4))
        gender (case (rand-int 3)
                 0 :fem 1 :fem 2 :neut)
        answer (gender (((pick cases) number) decl))]
    (println pick "," number ", declension " decl ", " gender)
    (print "Responsum tua: ")
    (let [input (read-line)]
      (if (= input answer)
        (println "Correct!")
        (println "Wrong! Answer is" answer)))))

