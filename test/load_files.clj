(ns load-files
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defmacro read-file [fname]
  (slurp (io/resource fname)))