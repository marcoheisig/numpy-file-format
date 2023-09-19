(defsystem :numpy-file-format
  :description "Read and write Numpy .npy and .npz files."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("nibbles"
   "alexandria"
   "trivial-features")

  :components
  ((:file "packages")
   (:file "dtypes")
   (:file "python-parser")
   (:file "load-array")
   (:file "store-array"))

  :in-order-to ((test-op (test-op :numpy-file-format/tests))))

(defsystem :numpy-file-format/tests
  :depends-on
  ("numpy-file-format"
   "uiop")

  :components
  ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :numpy-file-format/tests '#:run)))
