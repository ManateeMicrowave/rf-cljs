(ns network-touchstone-test
  (:require [rf-cljs.network.touchstone :refer [parser]]
            [cljs.test :refer-macros [deftest is testing]]))

(def foo "! This is a comment")

(parser foo)

(deftest comment-test
  (is (= [:Comment "! This is a comment"]
         (parser foo))))
