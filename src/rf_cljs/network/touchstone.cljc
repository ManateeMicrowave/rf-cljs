(ns rf-cljs.network.touchstone
  (:require [instaparse.core :as insta]
            [rf-cljs.misc.embed-resource :refer [embed-resource]]))

(def parser (insta/parser (embed-resource "rf_cljs/network/touchstone_grammar.ebnf")))

(parser "[good]")
(parser "! comment")

(parser "#")
(parser "[Version] 2.0")
(parser "# GHz S RI R 100")
(parser "# Y kHz R 100 MA")

(parser "[Number of Ports] 4")

(parser "[Two-Port Data Order] 12_21")
(parser "[Two-Port Data Order] 21_12")

(parser "[Number of Frequencies] 007")

(def normal-number 100)
(parser (str "[Number of Frequencies] " normal-number))

(parser "[Number of Frequencies] 000000000000000007")

(parser "[Reference]\n1.2 3.4 5.4")
(parser "[Reference]\n1.2\n 3.4\n 5.4")
(parser "[Reference] 1.2 3.4\n 5.4")
(parser "[Reference] 1.2 3.4 5.4")

;; (parser "[Reference] -1")
;; (parser "[Reference] like a word")

(parser "[Matrix Format] Full")
(parser "[Matrix Format] Lower")
(parser "[Matrix Format] Upper")

(parser
 "[Network Data]\n 5.00000 0.60 161.24 0.40 -42.20 0.42 -66.58 0.53 -79.34 \n 0.40 -42.20 0.60 161.20 0.53 -79.34 0.42 -66.58 \n 0.42 -66.58 0.53 -79.34 0.60 161.24 0.40 -42.20 \n 0.53 -79.34 0.42 -66.58 0.40 -42.20 0.60 161.24 \n ")
