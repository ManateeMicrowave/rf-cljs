(ns rf-cljs.core
  (:refer-clojure :exclude [+ * /])
  (:require
   ["mathjs" :as mathjs]
   [rf-cljs.networks.network :as network]
   [rf-cljs.math.operations :refer [acos cos square + hypot norm log10 abs * /]]
   [rf-cljs.math.complex :as complex]
   [rf-cljs.math.matrix :as mat]))
