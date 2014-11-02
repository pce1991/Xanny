(ns xanny.nlp
     (:use [opennlp.nlp]
           [opennlp.treebank]
           [clj-wordnet.core]
           [xanny.utilities])
     (:require [clojure.string :as string]))
;========================================================================================
;========================================================================================
;OPENNLP
;========================================================================================
;========================================================================================

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

;========================================================================================
;========================================================================================
;WORDNET
;========================================================================================
;========================================================================================
(def apachePOS->wordnetPOS 
  {#"N.*" :noun
   #"V.*" :verb
   #"J.*" :adjective
   #"RB.*":adverb})

(def wordnet (make-dictionary "NLP/WordNet-3.0/dict"))

(defn wn 
  "Return all entries for a word."
  ([word] (if-not (empty?  (wordnet word))
            (wordnet word)
            nil))
  ([word pos] (if-not (empty? (wordnet word pos))
                (wordnet word pos)
                nil)))

;;; if type is map, then its a single entry, if its a seq then concat
(defn lemmas [word & pos]
  "Returns a sequence of lemmas paired with their POS."
  (let [entry (if pos (wn word (first pos)) (wn word))]
    (if (seq? entry)
      (distinct (map (fn [ent] [(:lemma ent) (:pos ent)]) entry))
      (:lemma entry))))

(defn lemma [word pos]
  "This takes the POS and so should return only a single lemma pair, and if it can't do that satisfactorally it returns nil."
  (if (= 1 (count  (lemmas word pos)))
    (first (lemmas word pos))
    nil))

;;; might want to get POS for the words so that the lemma is precise
;;; include words in here that have no lemmas so that the sentence is indexed at least, losing tokens. 
;;; this is a problem because the position of the words as I'm getting them. I can just filter what appears,
;;; I shouldnt lose any data. 
(defn lemmatize [sentence]
  "Returns words mapped to all its possible lemmas."
  (let [words (tokenize (clojure.string/lower-case sentence))]
    (loop [w words
           lemma-map {}]
      (if (empty? w)
        (into {} (filter (fn [e] (not (nil? (val e)))) lemma-map))
        (recur (rest w) (conj lemma-map [(first w) (lemmas (first w))]))))))


(defn lemmatize-pos [sentence]
  "The same as lemmatize, except it takes into accont the POS of each word for when it gets the lemma."
  (let [words (pos-tag (tokenize (clojure.string/lower-case  sentence)))]
    (loop [w words
           lemma-map {}]
      (if (empty? w)
        (into {} (filter #(not (nil? (val %))) lemma-map))
        (recur (rest w)
               (conj lemma-map [(get (first w) 0) 
                                (if-let [matching (get-regex-key apachePOS->wordnetPOS (get (first w) 1))]
                                  (lemma (get (first w) 0) matching) 
                                  nil)]))))))

;;; how to get synonym for a word with multiple entries? Build a map of synomyms for each type, {:noun [] :verb []}
(defn synonyms-all [word]
  )

(defn synonyms-sentecne [])

(defn glosses
  ([word] (map :gloss (wn word)))
  ([word pos] (map :gloss (wn word pos))))


(defn synsets [])

;;; treat word-net entries like "go_after" as "go afterss"
;========================================================================================
;========================================================================================
;MARKOV GENERATION
;========================================================================================
;========================================================================================

;for trigrams i want how many times each word follows another, but base probabilities on
;the two previonus words barks | the, dog
;will my map be different? it could still pair words, but then how would i know the 
;probability two words... if start= the then there's a %50 dog %50 cat, from there the next
;dog it might go %50 run %50 bite, cat %25 run %25 claw %50 jump then the prob of run is given by 
;the two previous words. 
;this is what i want: x might follow y %50 of the time, and z follows x %25 of the time, but 
;%10 if x follows y, i need word pairs for that, or how do i represent it? 

(def names (string/split "Abas Ableros Acamas Adamas Adrestus Adrestus Aenius Aesepus Aesymnus Agastrophus Agelaos Agelaus Alastor Alcandrus Alcathous Alcmaon Amopaon Amphiclus Amphimachus Amphion Amphoterus Anchialus Antiphates Antiphus Aphareus Apisaon Archeptolemos Areilycus Areithous Aresilaus Aretaon Aretus Asaeus Ascalaphus Asius Asteropaeus Astyalus Astynous Astypylus Atymnius Autonous Axylus Bathycles Bienor Calesius Cebriones Charops Chersidamas Chromius Cleitus Clonius Cocranus Coeranus Crethon Croesmus Daitor Damasus Dardanus Deicoon Democo Demoleon Demouchos Deucalion Diores Dolon Dolops Dresus Dryops Echeclus Echemmon Echepolus Echius Eioneus Elasus Elatus Elephenor Eniopeus Ennomus Epaltes Epeigeus Epistor Erylaus Erymas Euchenor Euippus Euphorbus Gorgythion Halius Harpalion Hector Helenus Hippodamas Hippolochus Hippomachus Hipponous Hippotion Hypeirochus Hypeiron Hyperenor Hypsenor Iamenus Iasus Ilioneus Imbrius Ipheus Iphidamas Iphinous Iphition Isus Laogonus Leocritus Leucus Lycaon Lyco Lycophontes Lycophron Maris Mecistus Medon Melanippus Melanippus Melanthus Menesthes Menesthius Menon Mermerus Mnesus Molion Morys Mulius Mydon Noemon Odius Oenomaus Oenomaus Oileus Ophelestes Opheltius Opites Oresbius Orestes Ormenus Orsilochos Orus Othryoneus Otus Pandarus Patroclus Pedaeus Peirous Peisander Perimus Periphas Periphetes Phaestus Phalces Phegeus Phereclus Phlaemenes Phylacus Pidytes Podes Polydorus Polyidus Polymelus Promachus Pronous Protho Prytanis Pylantes Pylon Pyraechmes Pyris Rhigmus Sarpedon Satnius Scamandrius Scedius Schedius Socus Sthenelaus Stichius Teuthras Thersilochus Thestor Thoas Thoon Thrasius Thrasymedes Thymbraeus Tlepolemus Trechus Tros Xanthus" #" " ); ["Hector" "Odysseus" "Achilles" "Ajax" "Hercules" "Paris"]
  )

(defn merge-gram [map addition]
  "The addition will be a map of {new-letter {following-letter count}} and will be merged onto the map by conjing the new-letter with its contents in the map, and merging the following letter with the count of it in new-letter in the map."
  (merge-with (fn [letters letter] (conj letters (merge-with + letters letter))) 
              map addition))

(defn n-gram [n coll]
  (partition n 1 coll))

;go through each letter of every word, whenever a letter is encountered that isnt in the map, add it to the map, you then keep track of the current letter, and see what follows it. So you add H to the map, setting current letter to H, then an e is encountered so you add it to H in map while incrementing the count if its already there, then set the current letter to E to see what follows it. Don't set the current letter if its the last char in a word. 
;use starts and stops
; use the given n-gram/partition function to patition the words first. 
(defn letter-bi-gram [words-vector]
  (loop [words (map string/upper-case  words-vector)
             map {:START {}}]
    (if (empty? words)
      map
      (recur (rest words) 
             (loop [characters (string-seq (first words)) ;(map s/capitalize  (map str  (string-seq (first words))))
                    started? false
                    m map]
               (if (= (count characters) 1)
                 (merge-gram m {(first characters) {:STOP 1}})
;                 (merge-gram m {:STOP {(first characters)  1}})
                 (if started? ;if it isnt started then add the first letter to start, but dont move past it yet. 
                   (recur (rest characters) true (merge-gram m {(first characters) {(second characters) 1}}))
                   (recur characters true (merge-gram m {:START {(first characters) 1}})))))))))

(defn percentage [n total]
  (* 100.0 (/ n total)))

;this will replace every int count with a decimal percentage
(defn generate-percentages [letter-map]
  "Replaces all the counts in a map with the percentage of their frequency."
  (zipmap (keys letter-map)  
          (for [following-letter-map (vals letter-map)]
            (into {} (for [letter following-letter-map]
                       [(first letter) (percentage (second letter) (reduce + (vals following-letter-map)))])))))

;;; generate-names. the liklihood of each letter is dependent on its frequency, given the frequency of the last word, which I think is the probability of the first and second / the prob of first. 

(defn pick-letter [following-letter-map]
  "Takes the map of following letter frequencies and returns a letter based on the percentage."
  (let [letters (into [] (keys following-letter-map))
        counts (into [] (vals following-letter-map))]
    (get letters (wrand counts))))

;;; needs to cut off at certain point to avoid long names, and extend short names like "ES" 
(defn generate-name [letter-map]
  (loop [word [(pick-letter (:START letter-map))]]
    (let [next-letter (pick-letter (letter-map (last word)))] 
      (if (= :STOP next-letter)
        (string/join "" word)
        (recur (conj word next-letter))))))
