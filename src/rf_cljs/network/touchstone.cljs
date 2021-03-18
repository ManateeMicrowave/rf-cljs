(ns rf-cljs.network.touchstone)

(def formats
  "Mapping of two-letter `str` to function taking two floats and returning complex number"
  {:RI #(cmplx/complex %1 %2)
  ;;  :MA #(cmplx/complex %1 %2)
  ;;  :DB #(cmplx/complex %1 %2)
   })

(defn read
  "Takes a string `file-contents` and outputs a network map with the following members:
   {
      :format <`str` representing how the two columns of floating point numbers are to be 
              combined to make one complex number>
      :param <`char` representing the type of parameter read>
      :data <n-freqs x n-ports x n-ports x 2 mathjs/matrix>
      :f <n-nfreqs

   }"
  [file-contents]
  file-contents)
