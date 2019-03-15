(cl:in-package #:numpy-file-format)

(defun write-array (array filename)
  (with-open-file (stream filename :direction :output :element-type '(unsigned-byte 8))
    ))
