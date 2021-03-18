(ns rf-cljs.math.matrix
  (:refer-clojure :exclude [+ - * / <])
  (:require ["mathjs" :as mathjs]
            [rf-cljs.math.complex :as cplx]
            [rf-cljs.math.operations :refer [abs + - * / <]]
            [cljs-bean.core :refer [->js ->clj]]))

(defn matrix?
  "Returns `true` if `m` is a mathjs Matrix"
  [m]
  (= (type m) mathjs/Matrix))

(defn -matrix-type [items]
  (if (matrix? items)
    :mat
    (if (or (matrix? (first items))
            (number? (first items))
            (cplx/complex? (first items))
            (boolean? (first items)))
      :mat
      (when (vector? (first items))
        (if (matrix? (first (first items)))
          :block
          :mat)))))

(defmulti matrix
  "Constructor for a mathjs matrix
  The constructor takes in a vec or a seq to build a n-dimensional matrix

  In the specific case a a 2D matrix, other matrices can be passed in to form a block matrix"
  #'-matrix-type)

(defn shape
  "Returns a vector of the shape of `m`"
  [m]
  (->clj (. m size)))

(defn reshape
  "Reshapes `m` into `shape`"
  [m shape]
  (. m reshape (->js shape)))

(defn zeros
  "Generates a matrix of zeros with `shape`"
  [shape]
  (mathjs/zeros (matrix shape)))

(defmethod matrix
  :mat
  [items]
  (mathjs/matrix (->js items)))

(defmethod matrix
  :block
  [items]
  (let [block-rows (count items)
        row-heights (for [row items]
                      (first (shape (first row))))]
    (doseq [i (range block-rows)
            :let [row (nth items i)]
            block row
            :let [n-rows (first (shape block))]]
      (assert (= n-rows (nth row-heights i)) "Blocks in each row must have the same number of rows"))
    (let [total-rows (reduce + row-heights)
          total-cols (reduce + (for [blk (first items)]
                                 (second (shape blk))))
          out-shape [total-rows total-cols]]
      (doseq [row items
              :let [cols (reduce + (for [blk row]
                                     (second (shape blk))))]]
        (assert (= total-cols cols) "Total column size must be consistent"))
      (let [out-mat (zeros out-shape)]
        (doseq [i (range block-rows)
                :let [row (nth items i)]
                j (range (count row))
                :let [m (nth row j)
                      [block-rows block-cols] (shape m)
                      start-row (reduce + (for [k (range 0 i)
                                                :let [first-block (first (nth items k))]]
                                            (first (shape first-block))))
                      start-col (reduce + (for [k (range 0 j)
                                                :let [m (nth row k)]]
                                            (second (shape m))))
                      row-idxs (matrix (range start-row (+ start-row block-rows)))
                      col-idxs (matrix (range start-col (+ start-col block-cols)))
                      idxs (mathjs/index row-idxs col-idxs)]]
          (. out-mat subset idxs m))
        out-mat))))

(defn to-vec
  "Converts a mathjs matrix into a clojure nested vector. Note, this only
  works for types in CLJS core, i.e. not complex"
  [m]
  (->clj (. m toArray)))

(defn squeeze
  "Removes extra singular dimensions
  `(squeeze (matrix [[[1]]]))` = `(matrix [1])`"
  [m]
  (mathjs/squeeze m))

(defn idx
  "Retrieves the value(s) of `m` at `idxs`

  There must be an `idx` per dimension of `m`.
  `idxs` can be iterables that will retrieve those slices from
  the `n`th dimension (where `n` is the position in the function call)

  The `:all` keyword will slice all available values for the dimension
  in that position.
  "
  [m & idxs]
  (let [dims (shape m)
        idxs (for [i (range (count idxs))
                   :let [ix (nth idxs i)]]
               (if (=  ix :all)
                 (range (nth dims i))
                 ix))]
    (. m subset (apply mathjs/index (map ->js idxs)))))

(defn dot-times
  "Element-wise multiplication"
  [x y]
  (mathjs/dotMultiply x y))

(defn dot-divide
  "Element-wise division"
  ([x]
   (mathjs/dotDivide 1 x))
  ([x y]
   (mathjs/dotDivide x y)))

(defn dot-pow
  "Element-wise power "
  [x y]
  (mathjs/dotPow x y))

(defn dot-equals
  "Element-wise equality"
  [x y]
  (mathjs/equal x y))

(defn diag
  "Generate a dense, diagonal matrix from vector `v` on optional
  diagonal `k` (defaults to the main diagonal)"
  [v & k]
  (apply mathjs/diag (matrix v) k))

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
        nums (matrix (into [] (take n (repeatedly cplx/random))))]
    (reshape nums shape)))

(defn -fill [shape value]
  (if (= (count shape) 0)
    value
    (for [_ (range (first shape))]
      (-fill (rest shape) value))))

(defn fill
  "Constructs a new `mathjs/matrix` of shape `shape` with
   `value` for every entry."
  [shape value]
  (matrix (-fill shape value)))

(defn smaller [x y]
  (mathjs/smaller x y))

(defn larger
  [x y]
  (mathjs/larger x y))

(defn any
  "Returns `true` if any of the elements of `mathjs/matrix m` are true."
  [m]
  (some true? (to-vec (flat m))))

(defn all
  "Returns `true` if all of the elements of `mathjs/matrix m` are true."
  [m]
  (every? true? (to-vec (flat m))))

(defn equals
  ([x y]
   (mathjs/deepEqual x y))
  ([x y eps]
   (every? true? (to-vec (flat (< (abs (- x y)) (+ (zeros (shape x)) eps)))))))

;; There are indeed more, but I'm getting bored
