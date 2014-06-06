(ns xanny.texts
  (:require [clojure.string :as string]
            [clojure.core.match :as match]
            [clojure.java.io :as io]
            )
  (:use
        ;[clojure.string]
        ;include clojure match
   [xanny.nlp]
   [xanny.utilities]))
;translate my scheme code here mostly. might need to use some nlp stuff. 

;==========================================================================================
;==========================================================================================
;PATHS
;==========================================================================================
;==========================================================================================

;allow the user to navigate this. 
;adapt this to show all the folders too.
(defn show-paths []
  (loop [directory (io/file "text-files/")
         files (file-seq directory)
         count 0]
    (println count ": " (first files))
    (if (not (empty? (rest files)))
      (recur directory (rest files) (inc count)))))

;==========================================================================================
;==========================================================================================
;WRITING to files
;==========================================================================================
;==========================================================================================

;just spit it. don't use append because it'll just add an extra map not inside the texts map
(defn write-map [path map]
  (with-open [wrt (io/writer path :replace true)]
    (doseq [entry map] ;this needs co go deeper to avoid an overflow error
      (.write wrt ()))))

;use this to write finnegans wake once the extra spaces are removed. lazy, I know.
;doesnt really help either it seems, the PDF conversion is fucked.
;write list to file, add new line after each entry
(defn write-text [path seq]
  (with-open [wrt (io/writer path :replace true)]
    (doseq [entry seq]
      (.write wrt (str entry "\n")))))

;==========================================================================================
;==========================================================================================
;EDITING AND FORMATTING TEXTS
;==========================================================================================
;==========================================================================================

(defn trim-blanks [text-seq] 
  "removes blankline from the front so title is always first"
  (loop [seq text-seq]
    (if (empty? (first seq))
      (recur (rest seq))
      seq)))

;rewrite these with vectors: less reversing. 

;signal an error if empty
;allow an option to toss the ending as in gutenberg, or to save it like in the bible.
(defn trim-front [text-path start toss?]
  "This strips the text up to a point 'start' and returns a seq from that point."
  (with-open [rdr (io/reader text-path :encoding "UTF-8")]
    (if (nil? start)
      (into [] (trim-blanks (line-seq rdr)))
      (loop [seq (line-seq rdr)]
        
        (if (empty? seq)
          (into [] (line-seq rdr))            ;if its never found, return the rdr
          (if (re-matches start (first seq)) ;if it is empty it hasnt found end, so just return seq
            (if toss?
              (into [] (rest seq))
              (into [] seq))               ;vector cause stream closes
            (recur (rest seq))))))))

(defn trim-back [text-seq end toss?]
   (if (nil? end)
      text-seq
      (loop [seq (reverse text-seq)]
        (if (empty? seq) ;if its empty the end isnt found, return the text unchanged. 
          text-seq 
          (if (re-matches end (first seq));if its empty the marker hasnt been found. 
            (if toss?
              (reverse (rest seq))
              (reverse seq))
            (recur (rest seq)))))))


(defn cut-consecutive-lines [text-seq]
  "reduces consecutive blanklines to a single one, thus avoidung incrementing sections several times it a text."
  (loop [seq text-seq
         new-seq ()]
    (cond (empty? seq) 
          (reverse new-seq)
          (and (empty? (first seq))  
               (empty? (second seq))
               (not (empty? (first new-seq))))
          (recur (rest (rest seq))
                 (conj new-seq (first seq)))
          ;check to make sure this isnt an odd blank, if it is, the last thing added to new-seq would be a blank
          (and (empty? (first seq)) (empty? (first new-seq)))
          (recur (rest seq) new-seq)
          :else (recur (rest seq)
                       (conj new-seq (first seq))))))

;sometimes consecutive lines mean something, so in that situation I should go through and delete them, but place
;a marker to show that there's a new part, section, whatever it indicates.
(defn make-chunks-markers [text-seq in-a-row marker] 
  "takes a string, and replaces a seq of blanklines with it to use in marking up a text."
  (loop [seq text-seq
         new-seq ()]
    (cond (empty? seq)
          (reverse new-seq)
          (and (> (count seq) in-a-row) (every? empty? (sublist seq 0 (dec in-a-row))))
          (recur (sublist seq (dec in-a-row)) 
                 (conj new-seq marker))
          :else (recur (rest seq) (conj new-seq (first seq))))))

;it'd be useful if this could either place a blank line where the chunk was, or remove it entirely. 
(defn cut-chunk [text-seq start until]
  "cuts a point at the first match, and extending a certain number of lines, or until a matching regex pattern is found. "
  (if start
    (loop [seq text-seq
           new-seq ()]
      (if (empty? seq)
        (reverse new-seq)
        (if (re-matches start (first seq))
          (if (number? until)
            (recur (nthrest seq (inc  until)) new-seq) ;inc: until = 1 should mean delete it and the next line
            ;else call nth until the end is found. do I toss it or not?
            (recur (loop [s seq]
                     (if (re-matches until (first seq))
                       (rest s) ;toss it
                       (recur (rest s))))
                   new-seq)) ;recur on a loop, but maybe there's a cleaner way. 
          (recur (rest seq) (conj new-seq (first seq))))))
    text-seq))

(defn lonely-lines [text-seq]
  "This removes all isolated empty-lines, useful in texts with double-spacing."
  (loop [seq text-seq
         new-seq ()]
    (cond (empty? seq)
          (reverse new-seq)
          (and (empty? (first seq)) ;if single blankline, kill it.
               (not-empty (second seq)))
          (recur (rest seq) new-seq) 
          :else (recur (rest seq) (conj new-seq (first seq))))))

;useful for the faerie queene. 
(defn remove-all-but-nth 
  "removes every line that matches pattern, except the nth occurence which is preserved."
  [text-seq pat n])

;use this for poetry like Whitman which has run-on lines. I think it'll work like: if a pattern is hit then that line is put into the previous line. Doesnt work!
(defn collapse-lines [text-seq pat]
  "Any string that matches the pattern gets collapsed into the previous line. "
  (if (nil? pat)
    text-seq
    (loop [seq text-seq
           new ()]
      (if-not (empty? seq) 
        (let [line (first seq)]
          (if (re-matches pat line)
            (recur (rest seq) (conj (rest new) (string/join " " [(first new) (string/triml line)])))
            (recur (rest seq) (conj new line))))
        (reverse new)))))



;;; use in shakespeare where stage-directions should be one line. give several predicates like justified and left-justified so it handles both instances without incrementing the section marker. 
(defn fold-lines [text-seq predicate]
  "This will fold the previous line into the one above it if there is not a blankline between them, assuming that both lines satisfy the predicate."
  (if (nil? predicate)
    text-seq
    (loop [seq text-seq
           new ()
           fold []]
      (let [line (first seq)]
        (cond (empty? seq)
              (reverse new)
              (and (predicate line) (or (empty? (second seq)) (not (predicate (second seq))))) ;if its a newline, of doesnt satisfy the predicate, then fold the lines and put in new.
              (recur (rest seq) (conj new (cut-gaps (string/join " " (conj fold line)))) []) 
              (predicate line)
              (recur (rest seq) new (conj fold line)) ;messes with formatting
              :else (recur (rest seq) (conj new line) []))))))

;adds a nil if it can't split the line, that's no good. 
;let it take a map of patterns and split-at so it can do multiple splits.
(defn cleave-lines [text-seq pat split-at]
  "Splits every line that matches the pattern at the given positon. Only use on lines you want in two parts, no more."
  (if (not (or (nil? pat) (nil? split-at)))
    (loop [seq text-seq
           new ()]
                                        ;(println new)
      (let [ln (first seq)]
        (if-not (empty? seq)
          (if (re-matches pat ln)
            (let [twane (string/split ln split-at)] ;loses whatever split-at is. 
              (if (= (count twane) 2)
                (recur (rest seq) (conj new (first twane) (second twane)))
                (recur (rest seq) (conj new (first twane))))) ;if it didnt split, avoid adding nil. 
            (recur (rest seq) (conj new (first seq))))
          (reverse new))))
    text-seq))

;this is useful for getting rid of superfluos ACT I SCENE 4, when I only need to know the act the first time it appears. incorporate into clear-text. 
;make this work for multiple patterns
(defn lop-lines
  "Deletes the target pattern in a string if it matches the general, but not the specific pattern. Target is a pattern to split by."
  [text-seq general-pat specific-pat target]
  (if (not (or  (nil? general-pat) (nil? specific-pat) (nil? target))) 
    (loop [seq text-seq
           new ()]
      (if-not (empty? seq)
        (let [line (first seq)] 
          (if (re-matches general-pat line)
            (if (re-matches specific-pat line)
              (recur (rest seq) (conj new line))
              (recur (rest seq) (conj new (string/triml (string/replace line target "")))))
            (recur (rest seq) (conj new line))))
        (reverse new)))
    text-seq))

;should I also use re-matches-one to make sure that it's registering and rmoving the proper one, not just the first hit. 
(defn remove-duplicates [text-seq pats]
  "Pat is a vector of regex patterns and it will remove duplicates for each of them."
  (if-not (empty? pats) 
    (loop [seq text-seq
           new ()
           counts (zipmap pats (repeat (count pats) 0))]
      (if (empty? seq)
        (reverse new)
        (let [line (first seq)] 
          (cond (and (re-matches-some pats line) (zero? (counts (which-matches? pats line)))) ;preserve the first ocuurence
                (recur (rest seq) (conj new line) (update-in counts [(which-matches? pats line)] inc))
                (and (re-matches-some pats line) (> (counts (which-matches? pats line)) 0))
                (recur (rest seq) new (update-in counts [(which-matches? pats line)] inc))
                :else (recur (rest seq) (conj new line) counts)))))
    text-seq))

;made for Shakespeare where comedy of errors is left justified. Might still have problems with stage direction though, and it'll push over act and scene markers. Maybe add an optional exceptions list. 
;modif this so it takes a map of starts and stops so it can perform multiple pushes in a single pass. 
(defn push-lines
  "Adds n number of spaces to all lines in a seq inbetween start and stop."
  [text-seq start stop n]
  (if (nil? start)
    text-seq
    (let [space-chunk (apply str (for [i (range n)] " "))] 
      (loop [seq text-seq
             new ()
             begun? false]
        (if (empty? seq)
          (reverse new) 
          (let [line (first seq)] 
            (cond (and (re-matches start line) (not begun?))
                  (recur (rest seq) (conj new line) true) ;dont push the beginnig which is usually a title. 
                  (and (re-matches stop line) begun?)
                  (recur (rest seq) (conj new line) false) ;dont push the end either. 
                  (and begun? (not (empty? line))) ;dont add spaces if its an empty
                  (recur (rest seq) (conj new (string/join "" [space-chunk line])) true)
                  :else (recur (rest seq) (conj new line) begun?))))))))

(defn pull-lines 
  "Pulls away n number of spaces to all lines, unless there are < n spaces, in which case it is just trimmed."
  [text-seq start stop n])

;let this take a map of regex keys and line values. Each time a key is found in the text, it's replaced with the line mapped to it. Use this in Shakespeare for replacing weird scene and act markers. The problem is that the line will have to be specific, but I want it to be general, so {#"ACT_.*" "ACT x"} So that it preserves whatever the number is. Maybe use a sentence-rest which will give me everthing after the regex pattern
(defn replace-lines [text-seq replacements])

;use on latin dict to separate entries which are each two lines. 
(defn insert-every-nth-line [text-seq addition n]
  "Inserts an item into the sequence every nth line."
  
  )

;just pass in nil for cut-start and until if you dont need anything cut
;ugly as sin. MAKE SURE THE ORDER IS RIGHT, it'll be unpleanant otherwise.
(defn clear-text [text-path start end toss? cut-start cut-until split-this split-at lop-gen lop-spec lop-split
                  collapse-pat fold-pred duplicate-pats push-start push-stop push-n]
  (push-lines 
   (remove-duplicates 
    (fold-lines 
     (collapse-lines 
      (cleave-lines
       (lop-lines 
        (cut-consecutive-lines 
         (cut-chunk 
          (trim-back (trim-front text-path start toss?) end toss?) ;trim front is the root, converting path to seq
          cut-start cut-until))
        lop-gen lop-spec lop-split) 
       split-this split-at)
      collapse-pat)
     fold-pred)
    duplicate-pats)
   push-start push-stop push-n))


;create new file out of a segment of a file. give a start and a stop, either as words, lines
;this is the next thing to do so I can trust the maps I have.

;find author's name and title, use this to mark start. in shae these mark
;volumes, but also the beginning 



;==========================================================================================
;==========================================================================================
;READING from files
;==========================================================================================
;==========================================================================================

(defn read-map [map-path])

(defn line-count [text-path]
  (with-open [rdr (io/reader text-path)]
    (let [seq (line-seq rdr)]
      (count seq))))

(defn compare-index [index1 index2]
  (cond (not (= (index1 0) (index2 0)))
        (- (index1 0) (index2 0))
        (not (= (index1 1) (index2 1)))
        (- (index1 1) (index2 1))
        (not (= (index1 2) (index2 2)))
        (- (index1 2) (index2 2))
        (not (= (index1 3) (index2 3)))
        (- (index1 3) (index2 3))
        :else (- (index1 4) (index2 4))))

;danger, gotten null pointer here, evaluate after cheching eof.
(defn file-end? [line]
  (re-matches #"End of[the]? Project Gutenberg.*" line))

(defn marker? [str marker] ;give as list, put into vector
  "accepts a string and marker, either a function or regex, returning true if 
the str matches the pattern, or it returns function applied to str." 
  (if-not (nil? marker)
    (if (pattern? marker)
      (re-matches marker str)
      (marker str))
    nil))

;;; not nil, empty, return
(defn segment? [str]
  )

;add a title and author entry for the first two lines. sometimes its on one line, so split at ", by" it its there.


(defn map-text 
  [text-path volume? book? part? section? segment? map-section-marker? ;either t or f. use 1 or 0 instd
   & {:keys [start end toss? cut-start cut-until split-this split-at lop-gen lop-spec lop-split
             collapse-pat fold-pred duplicate-pats push-start push-stop push-n] 
      :or {start #"Produced by.*|This etext was prepared by.*"
           end #".*(?i)End of\s?(the)? Project Gutenberg.*" 
           toss? true
           cut-start nil
           cut-until nil
           split-this nil
           split-at nil
           lop-gen nil
           lop-spec nil
           lop-split nil
           collapse-pat nil
           fold-pred nil
           duplicate-pats nil
           push-start nil
           push-stop nil
           push-n 0}}] ;this isn't matching all
  "returns a map of the text with keywords representing part, section, and segment.s"
  (loop [seq (clear-text text-path start end toss? cut-start cut-until split-this split-at lop-gen lop-spec
                         lop-split collapse-pat fold-pred duplicate-pats push-start push-stop push-n) 
         map {}
         volume 0
         book 0
         part 0
         section 0
         segment 0]
;    (println (first seq))
    (let [line (first seq)]
      (cond (or (nil? line) (file-end? line)) 
            (into (sorted-map-by compare-index) map) 
            ;not 0 based, causing moby to map weird!
            (marker? line volume?)
            (recur (rest seq)
                   (conj map [[(inc volume) 0 0 0 0] line])
                   (inc volume) 0 0 0 0)
            (marker? line book?)
            (recur (rest seq)
                   (conj map [[volume (inc book) 0 0 0] line])
                   volume (inc book) 0 0 0)
            (marker? line part?) ;COND 1
            (recur (rest seq)
                   (conj map [[volume book (inc part) 0 0] line])
                   volume book (inc part) 0 0)
            (and (marker? line section?) map-section-marker?) ;COND 2
            (recur (rest seq)
                   (conj map [[volume book part (inc section) 0] line])
                   volume book part (inc section) 1) ;put next segment at one instead of 0
            (marker? line section?) ;section not to be mapped.
            (recur (rest seq)
                   map ;don't do anything to the map, section increases, but next line will be at 0.
;(conj map [[volume book part (inc section) 0] ""]) ;easier to deal with than two cases for nil
                   volume book part (inc section) 0)
            (marker? line segment?) ;COND3
            (recur (rest seq)
                   (conj map [[volume book part section segment] line])
                                        ;right now only cleaning this up so as to avoid messing with the formatting of markers. 
                   volume book part section (inc segment))
            :else (recur (rest seq) map volume book part section segment))))) ;dont map if NotA

;doesn't work for sorting map it seems. 

;maybe write something to edit a map, for things like prose where I want sentences to be combined and then mapped, which might be easier to do if I already have a map, I could just use retrieval functions and then make new entries based on combining successive entries. 

;this needs to handle parentheticals, as well as lines ending in a -; the words need to be combined. 
;separate "a - b" into separate sentences? if so, where does the - go? get-sentences is getting in my way. just use
;.split
;TOSS THIS in favor of clearing the text in a specicfic way for prose, then just mapping normally. It'd be clearer than mapping once, and then remapping. 
(defn map-sentences [map] 
  (loop [seq (seq map) ;cdr this down
         map {}
         volume 0
         book 0
         part 0
         section 0
         paragraph []]
;part change
;    (println part section "and" (first (first seq)))
    (if (not (empty? seq))
      (cond (not (= (get (first (first seq)) 0) volume))
            (recur (rest seq)
                   (conj map (first seq))
                   (inc volume) book 0 0
                   paragraph) ;hold paragraph over???
            (not (= book (get (first (first seq)) 1)))
            (recur (rest seq)
                   (conj map (first seq))
                   volume (inc book) 0 0
                   paragraph)
            
            (not (= part (get (first (first seq)) 2) ))
            (recur (rest seq)
                   (conj map (first seq))
                   volume book (inc part) 0
                   paragraph) ;retain paragraph

            (not (= section (get (first (first seq)) 3))) 
            (if (not (= (count paragraph) 
                        (count (remove (fn [x] (not (indented? x))) paragraph))))
              ;;if the whole paragraph isnt lyrical, else map it by line
              (let [sentences (get-sentences (string/join " " paragraph))]
                (recur (rest seq)
                       (conj map (zipmap ;what if this is nil...
                                  (into [] (for [i (into []  (range (count sentences)))]
                                             [volume book part section i]))
                                  sentences))
                       volume book part (inc section) 
                       [(second (first seq))] ))
              ;ELSE
              (recur (rest seq)
                     (conj map (zipmap (into [] (for [i (into [] (range (count paragraph)))]
                                                  [volume book part section i]))
                                       (map cleanup-string  paragraph)));why no work?
                     volume book part (inc section)
                     [(second (first seq))]))
            ;if its justified, dont add to paragraph, just put in map
            ;what if there's already something in paragrapch?
            :else (recur (rest seq)
                         map volume book part section
                         (conj paragraph (second (first seq)))))
      (into (sorted-map-by compare-index) map))))  
;this doesnt need to be sorted, only used it to get here.

;since poem maps wont go through this process make a funtion to clean up all the entries.

;assumes only one extra section, need to dec more if > 2 linebreaks
(defn dec-sec [index]
  "Decrements every sec value in the index so blanks aren't sec 0 in some texts."
  (if (and (vector? index) (> (index 2) 0))
    (assoc index 2 
           (dec (index 2)))
    index))

;might not be necessary with clear-text taking care of superfluous newlines
(defn clean-map [m]
  (into (sorted-map-by compare-index) 
        (zipmap (map dec-sec (keys m))
                (vals m))))

;what to do about contractions? get rid of other symbols. use iterate?
;TOO slow. use split? this is GACK. also inconsistent. 
;use map instead of text-path, just grab vals
(defn make-corpora [text-path]
  "generates a map of words keyed to their count."
  (with-open [rdr (io/reader text-path)]
    (loop [seq (line-seq rdr)
           map {}]
      (if-not (empty? seq)
        (recur (rest seq) 
               (merge-with + map
                           (into {} 
                                 (for [word (tokenize (string/lower-case (first seq)))]
                                   [(string/replace (string/capitalize word)
                                                    #"[\"\_\(\-\!\']" "")
;placeholder til tokenizer is clean. should just trim front to leave hey--you alone. thats a tagger problem
                                    (element-count
                                     (tokenize (string/lower-case (first seq))) word)]))))
        (select-keys map
                     (for [[key val] map :when
                           (not (re-matches #"[\'\,\.\"\!\?]" key))]
                       key))))))
                                        ;this ensures duplicate entries have the whole count

;sence I'm using map I should keep track of where the words occur. so each word is a key tied to a map of count
;and a :index key containg a vector of all occurences indexed. 
;handle lemmatization
(defn make-corpora2 [map]
  ()
)


;merge corpora

;do lemmatization on corpora

;==========================================================================================
;==========================================================================================
;MAPPING
;==========================================================================================
;==========================================================================================

(def bible-books ["ECCLESIASTES" "SOLOMON" "HOSEA" "JOEL" "AMOS" "OBADIAH" "JONAH" "MICAH" "NAHUM" "HABAKKUK" "ZEPHANIAH" "HAGGAI" "ZECHARIAH" "MALACHI" "EZRA"  "PROVERBS"  "THE LAMENTATIONS OF" "LAMENTATIONS" "THE GOSPEL ACCORDING TO" "GOSPEL" "THE ACTS OF THE APOSTLES" "ACTS" "EPISTLE OF" "EPISTLE" "EPISTLE GENERAL OF" "THE REVELATION OF" "REVELATION" "GENESIS" "EXODUS" "NUMBERS" "LEVITICUS" "DEUTERONOMY" "MOSES" "THE BOOK OF" "BOOK" "PSALMS" "SAMUEL" "THE KINGS" "KINGS" "CHRONICLES"])


;this searches for indivudal words and not phrases like "SONG OF SOLOMON." 
(defn bible-book? [str]
  (if (not (empty? (filter string? 
                          (for [word (tokenize str)]
                            (some #{word} bible-books)))))
    true
    false))

;null pointer??? clean-line should work now
;OT is vol 0, NT is 1
(defn map-bible []
  (map-text "text-files/the-holy-bible/KJV-text-only.txt"
            nil
            #"THE NEW TESTAMENT OF OUR LORD AND SAVIOUR JESUS CHRIST"
            (fn [line] ;if it isn't allcaps then it isn't a book title. 
              (if (uppercase? line) (bible-book? line) false)) 
            #"[CHAPTER PSALM].*"
            (fn [line] (and (not (empty? line)) (not (newline? line))))
            true :start nil :end nil))


(defn map-dictionary []
  "returns a map with keys tied to seqs containing pronunciation key value vectors"
  (with-open [rdr (io/reader "text-files/websters.txt" :encoding "UTF-8")]
    (loop [seq (line-seq rdr) ;clear text here
           map {}]
      (let [line (first seq)]
        (if (empty? seq)
          map
          (if (uppercase? line)
            (recur (rest seq)
                   (merge-with concat map {line {:pronunciation (second seq)}})) ;add a versions thing so horse n isnt lost to v. 
            (recur (rest seq) map) ;normally would add to definition
))))))

;so some words are not a blank apart, this is a problem. maybe check if last char is a period? it its a blank line or ends with a peiod, then map. make sure this is an okay assumtion. if the count of ending peiods is the same as the number of entries, then it's probably safe. revise this by ignoring linebreaks, and mapping/starting a new entry only when a sentence-end? in found. 
;ignore lines that start with a number if there is no entry, this is a problem for agricola
(defn map-dictionary-latin []
  (with-open [rdr (io/reader "text-files/cassells-latin.txt" :encoding "UTF-8")]
    (loop [seq (clear-text  "text-files/cassells-latin.txt" #"Latin-English dictionary.*"
                            #"ENGLISH-LATIN.*" true nil nil nil nil nil nil nil nil nil nil nil nil nil)
           map {}
           word nil
           entry []]
      (if-not (empty? seq)
        (let [line (first seq)]
          (cond
           ;if its a newline, and an entry is accumulated, map it.
           (and (empty? line) (not (nil? word))) ;this is so multiple empties dont add to map
           (recur (rest seq) (assoc map (trim-punctuation  word) (string/join " " entry)) nil [])
           ;if its just an empty line and there's no word/entry, just move on. 
           (empty? line) ;toss empty lines if there is no word or entry
           (recur (rest seq) map word entry)
           ;if its page number, or a header, throw it out. 
           (or  (< (count line) 5) (integer-string? line)) 
           (recur (rest seq) map nil []) ;or will this overwrite when it shoudnt?
           ;if there isnt an entry or word, start one. 
           (and (nil? word) (empty? entry))
           (recur (rest seq) map (first-word line) (conj entry (rest-words line)))
           :else (recur  (rest seq) map word (conj entry line))))
        map))))

;==========================================================================================
;==========================================================================================
;MAP RETRIEVAL AND ACCESS
;==========================================================================================
;==========================================================================================

;(write-map "text-maps/texts-maps.clj" texts)

;FUCK THESE! I can use select-keys in one general function which will return a sub-map

;these all rely on text being mapped properly. it'll fail if some indexes are not mapped, such as
;if it fails to map a first segment, but maps the rest, it'll fail to retrieve it even though
;part of it is there. should count be incremented? the problem is sometimes it should start at
;1 if the marker is an empty line, but at 0 or there's something there. changing the sec and prt to 0 screwed some stuff up, I think 'cause of checks on empty? changed it back so lets find problem. I want markers when I print, indexes also, but maybe just for random. for everything else
;it would be appropiate to only show the units that changed. 

;have it also return index? or maybe just segment number, and for the others return appropiate value. 

(defn key-match [index1 index2]
  )

;let this be arbitrary so you can say something like give me the first sentence of every second paragraph from
;every volume in the first part.
;doesnt work with '_ in the vector. 

;thanks to Andrew Marshall
(defn fetch
  "Takes a map `m` keyed by vectors and a map `coords`
  keyed by indices into those vectors.

  Returns a submap of `m` that includes all on only the keys
  which agree with `coords` on the indices mentioned in `coords`."
  [m coords]
  (into (sorted-map-by compare-index)
    (for [[k :as e] m
          :when (every? #(= (nth k %) (get coords %))
                  (keys coords))]
      e)))


(defn dict-key-search [dict pat]
  (select-keys dict (into [] (for [[k v] dict
                                   :when (re-matches pat k)]
                               k))))

(defn dict-entry-search [dict pat]
  (select-keys dict (into [] (for [[k v] dict
                                     :when (re-matches pat v)]
                               k))))


;did remving the decs mess anything up?? it did!!!!!
(defn seg [map vol prt sec seg]
  (map [vol prt sec seg]))

;maybe put it in a vector rather than a list. filter out blanklines as well
(defn seg-seq [map vol prt sec start end]
  (remove nil? 
          (for [i (range start (inc end))]
            (seg map vol prt sec i))))

(defn seg-count [map vol prt sec]
  (loop [segment 0] 
    (if-not (nil? (seg map vol prt sec segment))
      (recur (inc segment))
      segment)))

(defn sec [map vol prt sec]
  (seg-seq map vol prt sec 0 
           (seg-count map vol prt sec)))

;filter out empties in this
(defn sec-seq [map vol prt start end]
  (remove empty? 
          (for [i (range start (inc end))]
            (sec map vol prt i))))

(defn sec-count [map vol prt]
  (loop [section 1]
    (if-not (empty? (sec map vol prt section)) 
      (recur (inc section))
      section))) ;includes the marker at section 0

(defn prt [map vol prt] 
  (sec-seq map vol prt 0 (sec-count map vol prt)))

(defn prt-seq [map vol start end] 
;remove emp?
  (for [i (range start (inc end))]
    (prt map vol i)))

(defn prt-count [map vol]
  (loop [part 1]
    (if-not (empty? (prt map vol part)) ;empty???
      (recur (inc part))
      (dec part)))) ;dec?

(defn vol [map vol])

(defn whole [map]
  (prt-seq map 1 (prt-count map)))

;let this take in optional arguments which can act like ranges, so a random verse from the old
;testament, or from Luke or something. 
;GACK GACK icky inc and lets, ugly ifs. wholly gack. 
(defn rand-seg [map & [part section]]
  (if part
    (if section
      (seg map part section (inc (rand-int (seg-count map part section))))
      (let [section (inc (rand-int (sec-count map part)))] ;might this cause name conflicts?
        (seg map part section
             (inc (rand-int (seg-count map part section))))))
    (let [part (inc (rand-int (prt-count map)))
          section (inc (rand-int (sec-count map part)))]
      (seg map part section (inc (rand-int (seg-count map part section)))))))

;if none of optional vars are given it prints the whole thing. 
;have it also take in ranges, so how to determine if (print-entry map 1 1 10) is printing a segment, or a range of sections. 
;not printing markers or index, i should fix this in the retrieval functions. 
;this loses formatting like indentation of paragraphs. it would be helpful to list indexes to
 ;help make division clearer. add a way to print random segments. 
;print paragraphs, not indv lines have a way to detect if its line based or net (metadata)
;make it work with volume
(defn print-entry [map & [part section segment]]
  (cond (and part section segment)
        (println (seg map part section segment))
        (and part section)
        (doseq [i (sec map part section)] (println i))
        part
        (doseq [i (prt map part)]
          (doseq [j i]
            (println j)))
        :else
        (doseq [i (whole map)]
          (doseq [j i]
            (doseq [k j]
              (println k))))))

(defn print-random [map & [part section]] 
  (cond (and part section) 
        (println (rand-seg map part section))
        part
        (println (rand-seg map part))
        :else (println (rand-seg map))))

;useless prbabably
(defn text-loop [map ignore-markers?]
  )

;write something that will interweave an arbitrary number of texts. 

;write a "next" function which will display the next entry based on if you're in a section

;Search for segment by phrase or "contains," or for part or section by marker or contents.
(defn search-text [map target]
  (select-keys map 
               (for [[key val] map 
                     :when (re-find target val)]
                 key)))

;get something like 100 highest count, or lowest count beginning with a
(defn corpora-seq [corpora]
  )

;==========================================================================================
;==========================================================================================
;SYSTEM
;==========================================================================================
;==========================================================================================
;find a way to access maps in the working view, and load up new ones

;==========================================================================================
;==========================================================================================
;UI
;==========================================================================================
;==========================================================================================

;take notes, highlight, link one text to another. 

;way too slow to do all of these, just write it somewhere. this should really be a function map-all-texts
(def texts {"Bible" '(map-bible)
            ;; "Webster's Dictionary" (map-dictionary)
            "Homer" {"The Illiad" '(map-text "text-files/POETRY/illiad-pope.txt"
                                            nil nil #"BOOK.*" empty? (fn [line] (re-matches #"  .*" line)) false
                                            :start #"THE ILIAD." :end #"CONCLUDING NOTE." :toss? true
                                            :cut-start #".*Illustration:.*" :cut-until 3) ;FAILS! 
                     "The Odyssey" ""} ;I have to get a chunk remover first to word on illustrations
            "Vergil" {"The Aeneid" '(map-text "text-files/POETRY/the-aeneid.txt" nil nil #".*LIBER.*"
                                             empty? not-empty true :start #"AENEIDOS" :toss? true
                                             :cut-start #".*PUBLI VERGILI MARONIS" :cut-until 3)}
            "Beowulf" '(map-text "text-files/POETRY/beowulf.txt" nil nil nil nil not-empty false
                                :start nil :end nil)
            "Chaucer" {}
;            I dont think this is mapping segments and secions right. 
            "Shakespeare" {"Plays" '(map-text "text-files/shakespeare/shakespeare.txt" 
                                             (fn [ln] (and (uppercase? ln) (not (notched? ln))
                                                           (not (or (re-matches #"ACT.*" ln)
                                                                    (re-matches #".*SCENE.*" ln)
                                                                    (re-matches #"THE END" ln)
                                                                    (re-matches #".*EPILOGUE.*|PROLOGUE.*" ln)
                                                                    (re-matches #".*DRAMATIS PERSONAE.*" ln)
                                                                    (re-matches #"INDUCTION.*" ln)
                                                                    (re-matches #"SC_.*" ln)
                                                                    )))) 
                                             #"ACT .*|Act .*" 
                                             #"\ ?Scene .*|.*SCENE .*|.*PROLOGUE.*|.*EPILOGUE.*|.*INDUCTION.*" 
                                             
                                             (fn [line]
                                               (or (and  (not (indented? line)) (notched? line))
                                                   ;get stage directions here, but they arent always justified...
                                                   ;if its left or right justified it could be a stage direction, watch this ovrewriting something thought. this causes problems where a mult-line direction is counted as several sections. collapse-lines. I want it to collapse two left-jutifed lines that arens separated by a blankline.
                                                   (or  (justified? line) (left-justified? line))
                                                   ))
                                             not-empty
                                        ;notched? ;used to be indented?... 
                                             true ;dont map blank-lines
                                             :start #"1603" :toss? true 
                                             :cut-start #"<<THIS ELECTRONIC VERSION OF THE COMPLETE WORKS OF WILLIAM"
                                             :cut-until 7
                                             ;I need to split stage directions from lines as well. 
                                             :split-this #"(?i)ACT[\ \_][I II III IV V].*" ;split the odditiets too.
                                             :duplicate-pats [#"THE COMEDY OF ERRORS" #"THE TEMPEST"] ;others?
                                             :split-at #"\. "
                                             :fold-pred (fn [ln] (and (not (re-matches #"ACT.*|(?i)SCENE.*" ln)) 
                                                                  (or (justified? ln) (left-justified? ln))))
                                             ;lop should work now that the spec and split patterns are right. 
                                             :lop-gen #"(?i)ACT .*" 
                                             :lop-spec #"(?i)ACT [I IV V]+\.? SCENE [1 I].*"
                                             :lop-split #"ACT [I IV V]+\.?" ;should work now
                                             :push-start #"THE COMEDY OF ERRORS"
                                             :push-stop #"THE END" :push-n 2)

                           "Sonnets"  '(map-text "text-files/shakespeare.txt" nil nil nil integer-string? 
                                                (fn [line] ;(justified? line) ;kills heap space somehow
                                                  (re-matches #"  .*" line) )
                                                true :start #"1609" :end #"1603" :toss? true 
                                                :cut-start #"<<THIS ELECTRONIC VERSION OF THE COMPLETE WORKS OF WILLIAM"
                                                :cut-until 7)}

            ;;"Spencer" {}
            "John Milton" ;map his complete works, or split this up, which makes more sense: easier to access PL than volume 4 or something. DUH! map multiple texts by just setting a start and end point! do for shakespeare!
            {"Paradise-Lost" '(map-text "text-files/POETRY/paradise-lost.txt" nil nil #"  BOOK.*" 
                                       indented? (fn [line] (and (not-empty line) 
                                                                 (not (uppercase? line)))) false)
             
             "Paradise Regained" '(map-text "text-files/POETRY/paradise-regained.txt" 
                                           nil nil #"  THE.* BOOK" indented? not-empty true
                                           :start #"  John Milton")}
            ;;"Goethe" {"Faust I" {} "Faust II" {}}
            "Herman Melville" 
            {"Moby-Dick" '(map-sentences (map-text "text-files/PROSE/Moby-Dick.txt" 
                                                  nil #"ETYMOLOGY." #"CHAPTER.*|Epilogue" 
                                                  empty? not-empty false))}
            ;;make sure this indexing is adequete. 
            "Walt Whitman" '(map-text "text-files/POETRY/leaves-of-grass.txt" #"BOOK .*" left-justified?
                                     integer-string? empty? not-empty false
                                     :start #"By Walt Whitman"
                                     :collapse-pat #"\ {4,6}[0-9a-zA-Z].*");if the line is indented > 6, dont collapse
            
            ;; "James Joyce" {"Portrait of the Artist" "map here" "Ulysses" "Finnegans Wake"}
})


;broken. build a whole map-navigation system off of this. list all the keys of a text, find all the works of an author. Right now I need a way to determine if its an anonymous author, and so just a title-map, or if there's a map full of texts. 
(defn list-texts [] 
  (map (fn [k] (if (map? (texts k)) (list k (keys k)) k)) (keys texts)))
;write a loaded-texts map here so I'm not updating the list of funtions to load. 

(def loaded-texts {})

(defn load-text 
  ([author] 
     (def texts (assoc texts author (eval (texts author)))))
  ([author text]  
     (def texts (assoc-in texts [author text] (eval ((texts author) text))))))


(defn toc [text-map]
  ;"Finds the most outer index and then lists each of it. Can be nested to give you book and part like for the bible."
  )
