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

(defn preprocess
  [stream]
  (apply str
         (for [l (line-seq stream)]
           (-> l
               (str/split #"!")
               first
               (or "")
               (str/replace #"\s+" " ")
               (str/trim)
               give-newline))))

(def parse (comp parser preprocess))

(with-open [rdr (io/reader (io/resource "touchstone_test_files/SusMicrostripStub_TLineSim.s4p"))]
  (parse rdr))
