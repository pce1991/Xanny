(ns xanny.parse
  (:require [instaparse.core :as insta]))

;;; formatting is still an issue here: ignoring successive blanklines,
;;; odd indentation (like stage directions), etcetera.
;;; what's gained is encoding the documents formatting as a grammar rather
;;; than shoehorning it into a numerical index, preserving semantics.

;;; really want a better way to write grammars, or format strings at least
;;; just makes these generated grammars, not strings. 
(def GRAMMARS
  {:bible "testament = 'OLD TESTAMENT' book | 'NEW TESTAMENT' book
                       book = chapter book | chapter testament | chapter
                       chapter = verse chapter 
                       verse = text verse | text
                       text = #'[A-Za-Z]+'"
   :play ""
   :book-poem ""
   })

;;; generate new grammar from these bases, so you could use book-poem, but
;;; add that there are two parts, each of twelve books, something like that.
;;; just concat onto it, optionally naming it, permananently adding it to
;;; grammars.

;;; I want to be able to number things, especiall for the bible. A verse should be tagged as its number, not :verse

;;; I need a way to get the depth of a parsed text and list all the types
;;; of sections it has, and how to access them. allow numerical representation if desired.  

;;; parsing a text will take a supplied CFG as a string and parse the text according to it.
(defn parse-text [text grammar]
  ((insta/parser grammar grammar) text))
