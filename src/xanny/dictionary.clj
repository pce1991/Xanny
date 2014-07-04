(ns xanny.dictionary
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [xanny.utilities :refer :all]))

(def dict-key {:source "collegiate"
               :key "7ae2d3fb-ef6f-4d35-ade5-259b8948b88f"})

(def thes-key {:source "thesaurus"
               :key "8b20bde0-b4aa-4586-b9d1-3134f9d252f3"})


(defn fetch-word [source word]
  (let [data (xml/parse (str "http://www.dictionaryapi.com/api/v1/references/" (:source source) "/xml/"
                                   word "&key=" (:key source)))]
    data))

(defn extract-data [data]
  ;get content of the data, 
  ;loop th
  {:pronunciation :pos :definitions {}})

;how can I do cool things like regex searches, completing words, getting multiple results. 
