(ns rf-cljs.core
  (:refer-clojure :exclude [+ * /])
  (:require
   ["mathjs" :as mathjs]
   [rf-cljs.networks.abcd :refer [abcd to-s]]
   [rf-cljs.math.operations :refer [cos square + hypot norm log10 abs * /]]
   [rf-cljs.math.complex :refer [complex real]]
   [rf-cljs.math.matrix :as mat :refer [matrix to-vec apply-axis]]))

(def m (matrix (for [freq (range 1e9 12e9 100e6)
                     :let [lambda (/ 3e8 freq)
                           beta (/ (* 2 Math/PI) lambda)]]
                 (to-s (*
                        (abcd {:type :tline :Z0 100 :beta beta :l 0.01})
                        (abcd {:type :tline :Z0 10 :beta beta :l 1})
                        (abcd {:type :tline :Z0 30 :beta beta :l 0.1})
                        (abcd {:type :tline :Z0 40 :beta beta :l 0.1})
                        (abcd {:type :tline :Z0 3.14 :beta beta :l 0.01})
                        (abcd {:type :tline :Z0 2 :beta beta :l 0.01})) 50))))
