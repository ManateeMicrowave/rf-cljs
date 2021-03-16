(ns rf-cljs.math.filters
  (:require [rf-cljs.math.utils :as utils]
            [rf-cljs.utils.e-series :as eseries]
            [rf-cljs.networks.abcd :as abcd]
            ["mathjs" :as mathjs]))

(defn butterworth
  "Returns the immittance coefficients for an `n`th order butterworth filter"
  [n]
  (into []
        (for [i (range 1 (inc n))]
          (* 2 (Math/sin (/ (* (dec (* 2 i)) Math/PI)
                            (* 2 n)))))))

(defn chebyshev
  "Returns the immittance coefficients for an `n`th order chebyshev filter
   `ripple` is to set the acceptable ripple in either `type` `:dB` or `:linear`"
  [n [ripple type]]
  (let [alpha (dec (case type
                     :dB (utils/db-to-linear ripple)
                     :linear ripple))
        beta (Math/sinh (/ (Math/atanh (/ 1 (Math/sqrt (inc alpha)))) n))
        a (butterworth n)]
    (loop [c [(/ (first a) beta)]
           i 1]
      (if (= i n)
        c
        (recur (conj c (/ (* (nth a i) (nth a (dec i)))
                          (* (nth c (dec i)) (+ (* beta beta)
                                                (Math/pow (Math/sin (/ (* i Math/PI)
                                                                       n))
                                                          2)))))
               (inc i))))))

(def filters
  {:butterworth butterworth
   :chebyshev chebyshev})

(defn scale
  "Scales a component `value` of `type` `:R` `:L` or `:C` by frequency `f` and impedance `z`"
  [value type f z]
  (/ (case type
       :C (/ value z)
       :R (* value z)
       :L (* value z))
     (* 2 Math/PI f)))

(def other-type
  {:C :L
   :L :C})

(def units
  {:C :F
   :L :H
   :R :Ohms})

(defn series-value [series [comp-val pf unit]]
  (let [value (utils/as-value [comp-val pf unit])
        exponent (Math/floor (Math/log10 value))
        mantissa (/ value (Math/pow 10 exponent))]
    (conj (utils/as-prefix (* (Math/pow 10 exponent) (eseries/nearest mantissa series)))
          unit)))

(defn lpf [{:keys [n fc z type series comp-first proto-args]}]
  (let [a (apply type n proto-args)
        types (take n (iterate other-type comp-first))]
    (for [i (range n)
          :let [component (nth types i)
                ai (nth a i)
                val (conj (utils/as-prefix (scale ai component fc z)) (units component))]]
      (if series
        (series-value series val)
        val))))

(defn H [C]
  (let [j (mathjs/complex "0+1im")
        * mathjs/multiply]
    (fn [omega] (abcd/abcd {:type :shunt :Y (* j omega C)}))))
