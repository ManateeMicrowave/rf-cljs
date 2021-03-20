(ns rf-cljs.network.touchstone
  (:require-macros [instaparse.core :refer [defparser]]
                   [rf-cljs.misc.embed-resource :refer [embed-resource]])
  (:require [instaparse.core :as insta]))

(defparser parser (embed-resource "rf_cljs/network/touchstone_grammar.ebnf"))

(def foo "! This is a comment")

(parser foo)
