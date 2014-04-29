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

;use efficient version
(defn transform
  [words]
  (->> words
       (partition 2 1) ;trigrams? not so easy... 
       (reduce (fn [acc [w next-w]]
                 (update-in acc
                            [w next-w]
                            (fnil inc 0))) ;fnil!
               {})))


;use n-grams from incantor
;leaves out last two words? fix or pad. inconsistent because He is different from he
(defn transform-tri
  [words]
  (->> words
       (partition 3 1)  
       (reduce (fn [acc [w next-w next-next]]
                 (update-in acc
                            [w next-w next-next]
                            (fnil inc 0))) ;fnil!
               {})))

;find a way to combinemerge first by conj, then conj again, and finally by add. not quite!

;find a way to merge these maps. i want the key to be the same, take its value which is a map,
;and combine the counts of all identical words. use merge-with + on the value maps. for each
;key, supply a new value which is the merge-with + of 

(defn transform-fast
  [words]
  (->> (map vector words (next words))
       (reduce (fn [acc [w1 w2]]
                 (let [c-map (get acc w1 (transient {}))]
                   (assoc! acc w1 (assoc! c-map w2
                                          (inc (get c-map w2 0))))))
               (transient {}))
       persistent!
       (reduce-kv (fn [acc w1 c-map]
                    (assoc! acc w1 (persistent! c-map)))
                  (transient {}))
       persistent!))

(defn markers [line]
  (let [new-line (str "*START* " line " *END*")]
    (clojure.string/split new-line #"\s+")))

;mine to take advantage of maps i already have. still has punctuation though, clean it up
;trim the quotes and other markings
(defn mark-text [map]
  (loop [sentences (vals map)
         sen-seq ()]
    (if-not (empty? sentences)
      (recur (rest sentences) 
             (concat (markers (first sentences)) sen-seq))
      sen-seq)))

;customize this for reading documents with starts that shouldnt be counted.
;seems to mark paragraphs as start instead of sentences. each line is a sentence
;grab from maps instead of files!
;deoparately need a better way to determine start and end!
(defn lazy-lines [file]
  (letfn [(helper [rdr]
            (lazy-seq
             (if-let [line (.readLine rdr)] ;need to cleanup line
               (concat (markers line) (helper rdr))
               (do (.close rdr) nil))))]
    (helper (clojure.java.io/reader file))))

;Mr. Hickey
(defn wrand   [slices]
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to slices."
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(defn generate-sentence [data]
  (loop [ws (data "*START*")
         acc []]
    (let [v (vec (vals ws))
          i (wrand v)
          w (nth (keys ws) i)
          nws (data w)]
      (if (= "*END*" w)
        (clojure.string/join " " acc)
        (recur nws (concat acc [w]))))))

;arity exeption on conj, out of bounds on nth, which baffles
;not sure about format. {"to" {"be" {"to" 1, "or" 2}}} means "to be or" is twice encountered,
;"to be to" is seen once.
;dont choose all 3 words at once, but how? pick cunnent word based on the last two, or given
;a word, choose next two? nope, cant skip, choose next based on previous two
(defn generate-sentence-tri [data]
  (loop [ws (data "*START*")
         acc []]
    (let [v (vec (vals (apply conj (vals ws))))
          i (wrand v)
          w (nth (keys (into {}  (vals ws))) i)
          nws (data w)]
      (if (= "*END*" w)
        (clojure.string/join " " acc)
        (recur nws (concat acc [w]))))))


;generate a table of trigrams.


;use trigrams on my favorite text in joyce

;generate new sentences to use as training data for odd results

