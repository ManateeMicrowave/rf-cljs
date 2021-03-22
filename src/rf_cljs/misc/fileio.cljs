(ns rf-cljs.misc.fileio
  (:require [clojure.core.async :as a :refer [go-loop]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [clojure.string :as str]))

(defn read-chunk [file offset length]
  (.arrayBuffer (.slice file offset (+ offset length))))

;; We are assuming that the file we read is in ASCII
(defn decode-chunk [chunk]
  (let [decoder (js/TextDecoder.)]
    (.decode decoder chunk)))

(defn <read-lines [file chunk-size]
  (let [ch (a/chan)]
    (go-loop [offset 0]
      (let [chunk-str (->> (read-chunk file offset chunk-size)
                           <p!
                           decode-chunk)
            this-size (count chunk-str)
            next-offset (loop [bytes 0
                               lines (let [lines (str/split chunk-str #"\n|\r|\r\n")]
                                       (if (or (= (->> (count chunk-str)
                                                       dec
                                                       (nth chunk-str))
                                                  \newline)
                                               (not= this-size chunk-size))
                                         lines
                                         (butlast lines)))]
                          (if (seq lines)
                            (do
                              (when (not= "" (first lines))
                                (a/>! ch (first lines)))
                              (recur (+ bytes (count (first lines)))
                                     (rest lines)))
                            (+ (count (re-seq #"\r|\n" chunk-str)) bytes)))]
        (if (not= this-size chunk-size)
          (a/close! ch)
          (recur (+ offset next-offset)))))
    ch))
