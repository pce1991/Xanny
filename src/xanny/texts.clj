(ns xanny.texts
  (:require [clojure.string :as string]
            [clojure.core.match :as match]
            [clojure.java.io :as io])
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

;put this into a vector so its easier to read from. write as .clj, append to a string so you
;dont have to .nter the whole path, just name
(defn write-map [path map]
  (with-open [wrt (io/writer path :replace true)]
    (doseq [entry map]
      (.write wrt (with-out-str (pr entry))))))

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
;Really needs to be solid if I'm ever going to convert my library from page to PDF to .txt


;this will be most useful for texts converted from PDFs I imagine. will also be used to
;remove gutenberg stamps, licenses, and other crap. 
;removes all gutenberg shit!

;make sure files are in UTF-8 formatting.

(defn trim-blanks [text-seq] 
  "removes blankline from the front so title is always first"
  (loop [seq text-seq]
    (if (empty? (first seq))
      (recur (rest seq))
      seq)))

;rewrite these with vectors: less reversing. 

;these give null pointers when the start or end isnt found. fix it

;these are reversing it if there are no start and end markers!

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
      (into [] (reverse seq))
      (loop [seq (reverse text-seq)]
        (if (empty? seq) ;if its empty the end isnt found, return the text unchanged. 
          text-seq 
          (if (re-matches end (first seq));if its empty the marker hasnt been found. 
            (if toss?
              (reverse (rest seq))
              (reverse seq))
            (recur (rest seq)))))))


;count consecutive line breaks and remove them. verify this works.
;isnt reducing everything to single line breaks. cant handle uneven number of lines.
;change this to remove chunks of a certain length and replace them with a cleaner marker.
(defn consecutive-lines [text-seq]
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
;this will be useful in marking up a text
;DOH, finns wake has a different problem, so make this more generally something to replace a chunk of text
(defn make-linechunks-markers [text-seq in-a-row marker] 
  "takes a string, and replaces a seq of blanklines with it to use in marking up a text."
  (loop [seq text-seq
         new-seq ()]
    (cond (empty? seq)
          (reverse new-seq)
          (and (> (count seq) in-a-row) (every? empty? (sublist seq 0 (dec in-a-row))))
          (recur (sublist seq (dec in-a-row)) 
                 (conj new-seq marker))
          :else (recur (rest seq) (conj new-seq (first seq))))))


;sanitize texts by removing lots of mhitespace fram a line, a common error in PDF conversion it seems. 

;cut single linebreaks out of text like finnegan's wake which has them between every two lines.
;after all lonely lines are removed, it can be run through consecutive-lines

(defn lonely-lines [text-seq]
  (loop [seq text-seq
         new-seq ()]
    (cond (empty? seq)
          (reverse new-seq)
          (and (empty? (first seq)) ;if single blankline, kill it.
               (not-empty (second seq)))
          (recur (rest seq) new-seq) 
          :else (recur (rest seq) (conj new-seq (first seq))))))

(defn clear-text [text-path start end toss?]
  (consecutive-lines (trim-back (trim-front text-path start toss?) end toss?)))


;clear out the top title and page number from a PDF text

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
        :else (- (index1 3) (index2 3))))

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

;mapping ulysses is a pain. play formatting, verse. check to see if it's justified
;use Nesting instead of this. might be more flexible with arbitrary number of indexes.
;IM NOT BEING CONSISTENT WITH ZERO INDEXING which is used for segments, but no others.
;moby chapter 1 is at 0, because chapter 1 is a new volume but also a chapter, would be easier
;if prt was 0 indexed. I do want that for parts, unless they have a name, and section titles are 
;mapped at 0. add map-part-marker? and figure out how to have a part be a volume marker to.
;add a smaller index than segment which is word.

;add a title and author entry for the first two lines. sometimes its on one line, so split at ", by" it its there.
;add fifth called "book" in between volume and part. Sh is many volumes , the bible is two books, 66 parts
;guess I also need a :toss option for trim-front/back
(defn map-text 
  [text-path volume? part? section? segment? map-section-marker? ;either t or f. use 1 or 0 instd
   & {:keys [start end toss] :or {start #"Produced by.*"
                                  end #"End of\s?(the)? Project Gutenberg.*"
                                  toss true}}] ;this isn't matching all
  "returns a map of the text with keywords representing part, section, and segment.s"
 
                                        ;(println (empty? (clear-text text-path start end)))
  (loop [seq (clear-text text-path start end toss) ;the problem is start and end aren't getting new val
         map {}
         volume 0
         part 0
         section 0
         segment 0]
    (let [line (first seq)]
      (cond (or (nil? line) (file-end? line)) 
            (into (sorted-map-by compare-index) map) 
            ;not 0 based, causing moby to map weird!
            (marker? line volume?)
            (recur (rest seq)
                   (conj map [[(inc volume) 0 0 0] line])
                   (inc volume) 0 0 0)
            (marker? line part?) ;COND 1
            (recur (rest seq)
                   (conj map [[volume (inc part) 0 0] line])
                   volume (inc part) 0 0)
            (and (marker? line section?) map-section-marker?) ;COND 2
            (recur (rest seq)
                   (conj map [[volume part (inc section) 0] line])
                   volume part (inc section) 1) ;put next segment at one instead of 0
            (marker? line section?) ;section not to be mapped.
            (recur (rest seq)
                   (conj map [[volume part (inc section) 0] ""]) ;easier to deal with than two cases for nil
                   volume part (inc section) 0)
            (marker? line segment?) ;COND3
            (recur (rest seq)
                   (conj map [[volume part section segment] line])
                                        ;right now only cleaning this up so as to avoid messing with the formatting of markers. 
                   volume part section (inc segment))
            :else (recur (rest seq) map volume part section segment))))) ;dont map if NotA

;doesn't work for sorting map it seems. 

;maybe write something to edit a map, for things like prose where I want sentences to be combined and then mapped, which might be easier to do if I already have a map, I could just use retrieval functions and then make new entries based on combining successive entries. 

;this writes back the nils that I want as empty strings!!! screws up retrieval
;this needs to handle parentheticals, as well as lines ending in a -; the words need to be combined. 
;separate "a - b" into separate sentences? if so, where does the - go? get-sentences is getting in my way. just use
;.split
(defn map-sentences [map] 
  (loop [seq (seq map) ;cdr this down
         map {}
         volume 0
         part 0
         section 0
         paragraph []]
;part change
;    (println part section "and" (first (first seq)))
    (if (not (empty? seq))
      (cond (not (= (get (first (first seq)) 0) volume))
            (recur (rest seq)
                   (conj map (first seq))
                   (inc volume) 0 0
                   paragraph) ;hold paragraph over???
            (not (= part (get (first (first seq)) 1) ))
            (recur (rest seq)
                   (conj map (first seq))
                   volume (inc part) 0
                   paragraph) ;retain paragraph
            (not (= section (get (first (first seq)) 2))) 
            (if (not (= (count paragraph) 
                        (count (remove (fn [x] (not (indented? x))) paragraph))))
              ;;if the whole paragraph isnt lyrical, else map it by line
              (let [sentences (get-sentences (string/join " " paragraph))]
                (recur (rest seq)
                       (conj map (zipmap ;what if this is nil...
                                  (into [] (for [i (into []  (range (count sentences)))]
                                             [volume part section i]))
                                  sentences))
                       volume part (inc section) 
                       [(second (first seq))] ))
              ;ELSE
              (recur (rest seq)
                     (conj map (zipmap (into [] (for [i (into [] (range (count paragraph)))]
                                                  [volume part section i]))
                                       (map cleanup-string  paragraph)));why no work?
                     volume part (inc section)
                     [(second (first seq))]))
            ;if its justified, dont add to paragraph, just put in map
            ;what if there's already something in paragraph?
            :else (recur (rest seq)
                         map volume part section
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
           #"THE NEW TESTAMENT OF OUR LORD AND SAVIOUR JESUS CHRIST"
            (fn [line] ;if it isn't allcaps then it isn't a book title. 
              (if (uppercase? line) (bible-book? line) false)) 
            #"[CHAPTER PSALM].*"
            (fn [line] (and (not (empty? line)) (not (newline? line))))
            true :start nil :end nil))

;map shakespeare. split the text up into several sequences, then map map-text over them.


(defn map-dictionary []
  )

;==========================================================================================
;==========================================================================================
;MAP RETRIEVAL AND ACCESS
;==========================================================================================
;==========================================================================================

;create a map of authors or works.
;NULL POINTER ON paradise lost. now why is that? similar to problem with map-bible, which was!?!?!
;oh, it was a problem with start and stop not getting values. not the problem here though :(
;read in all the maps in another funtion, but hardcode it now. title them from the title given in the metadata
(def texts {"John Milton"
            {"Paradise-Lost" (map-text "text-files/POETRY/paradise-lost.txt" nil #"  BOOK.*" 
                                       indented? (fn [line] (and (not-empty line) 
                                                                 (not (uppercase? line)))) false)

             "Paradise Regained" (map-text"text-files/POETRY/paradise-regained.txt" 
                                          nil #"  THE.* BOOK" indented? not-empty true)}
            "Herman Melville" 
            {"Moby-Dick" (map-sentences (map-text "text-files/PROSE/Moby-Dick.txt" 
                                                  #"ETYMOLOGY." #"CHAPTER.*|Epilogue" 
                                                  empty? not-empty false))}})

;FUCK THESE! I can use select-keys in one general function which will return a sub-map

;these all rely on text being mapped properly. it'll fail if some indexes are not mapped, such as
;if it fails to map a first segment, but maps the rest, it'll fail to retrieve it even though
;part of it is there. should count be incremented? the problem is sometimes it should start at
;1 if the marker is an empty line, but at 0 or there's something there. changing the sec and prt to 0 screwed some stuff up, I think 'cause of checks on empty? changed it back so lets find problem. I want markers when I print, indexes also, but maybe just for random. for everything else
;it would be appropiate to only show the units that changed. 

;change to work with vectors!

;have it also return index? or maybe just segment number, and for the others return appropiate value. 

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

