(ns rf-cljs.math.matrix
  (:require ["mathjs" :as mathjs]
            [cljs-bean.core :refer [->js ->clj]]))

(defn matrix [& items]
  (mathjs/matrix (apply ->js items)))

(defn to-vec [m]
  (->clj (. m toArray)))

(defn shape [m]
  (->clj (. m size)))

(defn resize [m shape]
  (. m resize shape))

(defn squeeze [m]
  (mathjs/squeeze m))

(defn idx [m & idxs]
  (let [dims (shape m)
        idxs (for [i (range (count idxs))
                   :let [ix (nth idxs i)]]
               (if (=  ix :all)
                 [0 (dec (nth dims i))]
                 ix))]
    (. m subset (apply mathjs/index (map ->js idxs)))))

(defn dot-times [x y]
  (mathjs/dotMultiply x y))

(defn dot-divide [x y]
  (mathjs/dotDivide x y))

(defn dot-pow [x y]
  (mathjs/dotPow x y))

(defn apply-axis [m dim f]
  (mathjs/apply m dim f))

(defn broadcast [a f]
  (mathjs/map a f))

(defn col [m idx]
  (mathjs/column m idx))

(defn flat [m]
  (mathjs/flatten m))

(defn dot [x y]
  (mathjs/dot x y))

(defn cross [x y]
  (mathjs/cross x y))

(defn transpose [m]
  (mathjs/transpose m))

(defn ctranspose [m]
  (mathjs/ctranspose m))

(defn det [m]
  (mathjs/det m))

(defn random [shape & minmax]
  (matrix (apply mathjs/random (->js shape) minmax)))

;; There are indeed more, but I'm getting bored
