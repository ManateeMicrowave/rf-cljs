(ns rf-cljs.math.complex
  (:refer-clojure :exclude [number?])
  (:require
   ["mathjs" :as mathjs]))

(defn complex
  "Generates a complex number `re + i*im` from `re` and `im`"
  [re im]
  (mathjs/Complex (str re "+" im "i")))

(defn complex?
  "Test for number that is strictly complex"
  [c]
  (= "Complex" (. c -type)))

(defn real?
  "Test for number that is strictly real"
  [c]
  (cljs.core/number? c))

(defn number?
  "Test for a number type (complex or real)"
  [c]
  (or (real? c)
      (complex? c)))

(defn argument
  "Computes the argument of a complex value `x`
  For complex number `a+bi`, argument is computed as `(atan2 b a)`

  For matrices, the function is evaluted element wise"
  [x]
  (mathjs/arg x))

(defn conjugate
  "Computes the complex conjugate of complex value `x`
  For complex number `a+bi`, the conjugate is computed as `a-bi`

  For matrices, the function is evaluated element wise"
  [x]
  (mathjs/conj x))

(defn imaginary
  "Returns the imaginary component of complex value `x`

  For matrices, the function is evaluated element wise"
  [x]
  (mathjs/im x))

(defn real
  "Returns the real component of complex value `x`

  For matrices, the function is evaluated element wise"
  [x]
  (mathjs/re x))

(defn random
  "Generates a random complex number with optional `max` argument"
  ([] (complex (rand) (rand)))
  ([max] (complex (rand max) (rand max))))

(extend-protocol IPrintWithWriter
  mathjs/Complex
  (-pr-writer [c writer _]
    (write-all writer (str "#Complex[" (real c) " + " (imaginary c) "i]"))))
