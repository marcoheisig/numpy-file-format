(cl:in-package #:numpy-file-format)

(defun load-array-metadata (filename)
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    ;; The first 6 bytes are a magic string: exactly \x93NUMPY.
    (unless (and (eql (read-byte stream) #x93)
                 (eql (read-byte stream) 78)  ; N
                 (eql (read-byte stream) 85)  ; U
                 (eql (read-byte stream) 77)  ; M
                 (eql (read-byte stream) 80)  ; P
                 (eql (read-byte stream) 89)) ; Y
      (error "Not a Numpy file."))
    (let* (;; The next 1 byte is an unsigned byte: the major version number
           ;; of the file format, e.g. \x01.
           (major-version (read-byte stream))
           ;; The next 1 byte is an unsigned byte: the minor version number
           ;; of the file format, e.g. \x00.
           (minor-version (read-byte stream))
           (header-len
             (if (= major-version 1)
                 ;; Version 1.0: The next 2 bytes form a little-endian
                 ;; unsigned int: the length of the header data HEADER_LEN.
                 (logior (ash (read-byte stream) 0)
                         (ash (read-byte stream) 8))
                 ;; Version 2.0: The next 4 bytes form a little-endian
                 ;; unsigned int: the length of the header data HEADER_LEN.
                 (logior (ash (read-byte stream) 0)
                         (ash (read-byte stream) 8)
                         (ash (read-byte stream) 16)
                         (ash (read-byte stream) 24)))))
      (declare (ignore minor-version))
      ;; The next HEADER_LEN bytes form the header data describing the
      ;; array’s format. It is an ASCII string which contains a Python
      ;; literal expression of a dictionary. It is terminated by a newline
      ;; (\n) and padded with spaces (\x20) to make the total of len(magic
      ;; string) + 2 + len(length) + HEADER_LEN be evenly divisible by 64
      ;; for alignment purposes.
      (let* ((dict (read-python-object-from-string
                    (let ((buffer (make-string header-len :element-type 'base-char)))
                      (loop for index from 0 below header-len do
                        (setf (schar buffer index) (code-char (read-byte stream))))
                      buffer)))
             (fortran-order (gethash "fortran_order" dict))
             (dimensions (gethash "shape" dict)))
        (declare (ignore fortran-order)) ;; TODO
        (multiple-value-bind (element-type endianness)
            (parse-dtype (gethash "descr" dict))
          (values dimensions element-type endianness
                  (+ header-len (if (= 1 major-version) 10 12))))))))

(defun load-array (filename)
  ;; We actually open the file twice, once to read the metadata, and once
  ;; to read the file with a suitable element type.
  (multiple-value-bind (dimensions element-type endianness header-length)
      (load-array-metadata filename)
    (declare (ignore endianness)) ; TODO
    ;; Following the header comes the array data. If the dtype contains
    ;; Python objects (i.e. dtype.hasobject is True), then the data is a
    ;; Python pickle of the array. Otherwise the data is the contiguous
    ;; (either C- or Fortran-, depending on fortran_order) bytes of the
    ;; array. Consumers can figure out the number of bytes by multiplying
    ;; the number of elements given by the shape (noting that shape=()
    ;; means there is 1 element) by dtype.itemsize.
    (let* ((array (make-array dimensions :element-type element-type))
           (total-size (array-total-size array)))
      (etypecase array
        ((simple-array single-float)
         (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 32))
           (loop repeat (/ header-length 4) do (read-byte stream))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (ieee-floats:decode-float32
                    (read-byte stream))))))
        ((simple-array double-float)
         (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 64))
           (loop repeat (/ header-length 8) do (read-byte stream))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (ieee-floats:decode-float64
                    (the (unsigned-byte 64) (read-byte stream)))))))
        ((simple-array (unsigned-byte 64))
         (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 64))
           (loop repeat (/ header-length 8) do (read-byte stream))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (read-byte stream)))))
        ((simple-array (signed-byte 64))
         (with-open-file (stream filename :direction :input :element-type '(signed-byte 64))
           (loop repeat (/ header-length 8) do (read-byte stream))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (read-byte stream)))))
        ;; TODO more array types
        )
      array)))
