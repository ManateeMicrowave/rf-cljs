(ns rf-cljs.misc.embed-resource
  (:require [clojure.java.io :as io]))

(defmacro embed-resource [fname]
  (slurp (io/resource fname)))
