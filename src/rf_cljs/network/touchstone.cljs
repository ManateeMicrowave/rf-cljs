(ns rf-cljs.network.touchstone
  (:require-macros [rf-cljs.misc.embed-resource :refer [embed-resource]]
                   [instaparse.core :refer [defparser]])
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.core.async :as a]
            [rf-cljs.misc.fileio :as io]))

(defparser parser (embed-resource "rf_cljs/network/touchstone_grammar.ebnf"))

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

(defn <preprocess-file
  [file]
  (let [lines-chan (io/<read-lines file 4096)
        processed-lines-chan (a/chan)]
    (a/go-loop []
      (if-let [line (a/<! lines-chan)]
        (do
          (when-let [processed-line (preprocess-line line)]
            (a/>! processed-lines-chan processed-line))
          (recur))
        (a/close! processed-lines-chan)))
    processed-lines-chan))

(defn <parse [file]
  (let [lines-chan (<preprocess-file file)]
    (a/go-loop [lines []]
      (if-let [line (a/<! lines-chan)]
        (recur (conj lines line))
        (println (parser (apply str lines)))))))
