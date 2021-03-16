(ns rf-cljs.math.matrix
  (:require ["mathjs" :as mathjs]
            [cljs-bean.core :refer [->js ->clj]]))

(defn matrix [& items]
  (mathjs/matrix (apply ->js items)))

(defn to-vec [m]
  (->clj (. m toArray)))

(defn idx [m & idxs]
  (. m subset (apply mathjs/index (map ->js idxs))))

(defn shape [m]
  (->clj (. m size)))

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

;; There are indeed more, but I'm getting bored
