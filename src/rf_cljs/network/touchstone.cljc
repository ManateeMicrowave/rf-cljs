(ns rf-cljs.network.touchstone
  (:require [instaparse.core :as insta]
            [rf-cljs.misc.embed-resource :refer [embed-resource]]))

(def parser (insta/parser (embed-resource "rf_cljs/network/touchstone_grammar.ebnf")))

(def v1 (embed-resource "touchstone_test_files/example_9_v1.s1p"))

(parser v1)
