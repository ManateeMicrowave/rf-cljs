(ns rf-cljs.math.complex
  (:require
   ["mathjs" :as mathjs]))

(defn complex [re im]
  (mathjs/Complex (str re "+" im "i")))

(defn arg [x]
  (mathjs/arg x))

(defn conjugate [x]
  (mathjs/conj x))

(defn imaginary [x]
  (mathjs/im x))

(defn real [x]
  (mathjs/re x))

(defn random
  ([] (complex (rand) (rand)))
  ([max] (complex (rand max) (rand max))))
