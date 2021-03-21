(ns rf-cljs.network.touchstone
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [rf-cljs.misc.embed-resource :refer [embed-resource]]
            [clojure.java.io :as io]))

(def parser (insta/parser (embed-resource "rf_cljs/network/touchstone_grammar.ebnf")))

(defn give-newline
  [string]
  (when (> (count string) 0)
    (str string "\n")))

(defn preprocess-line
  "Process a line to spare the parser the worst parts of the touchstone format."
  [l]
  (-> l
      (str/split #"!")
      first
      (or "")
      (str/replace #"\s+" " ")
      (str/trim)
      give-newline))

(defn preprocess-file
  [file])

(def parse (comp parser preprocess))

(with-open [rdr (io/reader (io/resource "touchstone_test_files/example_5_v2.s4p"))]
  (parse rdr))
