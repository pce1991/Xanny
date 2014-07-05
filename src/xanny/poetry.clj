(ns xanny.poetry)

;this isnt quite the right way to say this programattically; what determines a feet is the relative stress, so essentially we are saying the first is greater than the second no matter that value. This could be expressed with > < functions where the first takes on any value less than the second. 
;more here http://en.wikipedia.org/wiki/Metrical_foot
(def feet {:iamb ["-" "/"] :trochee ["/" "-"] :anapest ["-" "-" "/"] :dactyl ["/" "-" "-"] :spondee ["/" "/"] :pyrrhus ["-" "-"]})

(def meter {:mono 1 :di 2 :tri 3 :tetra 4 :penta 5 :hexa 6 :hepta 7 :octa 8})

;variations in a line must contain leoo irregular feet than the dominate feet. 
;truncations happen more regularly at the beginnig and ends of line than in the middle, so give these a weight. 

;use the dictionary to count syllables in a word. Don't worry about meanings yet, just find words that fit meter. 
;given a line, count the syllables and provide a scan for it. 

;generate possible scansions of a line. Draw on the dictionary for this. 
