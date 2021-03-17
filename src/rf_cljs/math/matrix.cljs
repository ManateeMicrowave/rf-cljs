(ns rf-cljs.math.matrix
  (:require ["mathjs" :as mathjs]
            [rf-cljs.math.complex :as complex]
            [cljs-bean.core :refer [->js ->clj]]))

(defn matrix [& items]
  (mathjs/matrix (apply ->js items)))

(defn matrix? [m]
  (= (type m) mathjs/Matrix))

(defn to-vec [m]
  (->clj (. m toArray)))

(defn shape [m]
  (->clj (. m size)))

(defn reshape [m shape]
  (. m reshape (->js shape)))

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

(defn dot-equals [x y]
  (mathjs/equal x y))

(defn diag [v & k]
  (matrix (apply mathjs/diag (->js v) k)))

(defn apply-axis [m dim f]
  (mathjs/apply m dim f))

(defn broadcast [a f]
  (mathjs/map a f))

(defn col [m idx]
  (mathjs/column m idx))

(defn ones [shape]
  (matrix (mathjs/ones (->js shape))))

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

(defn inv [m]
  (mathjs/inv m))

(defn eye [shape]
  (mathjs/identity (->js shape))) ;->js converts to javascript version :'-)

(defn random [shape & minmax]
  (matrix (apply mathjs/random (->js shape) minmax)))

(defn random-complex [shape]
  (let [n (reduce * shape)
        nums (matrix (into [] (take n (repeatedly complex/random))))]
    (reshape nums shape)))

(defn equals [x y]
  (mathjs/deepEqual x y))

;; There are indeed more, but I'm getting bored
