(in-package #:numpy-file-format)

(defun load-array-metadata/stream (stream)
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
    (let ((dict (read-python-object-from-string
                 (let ((buffer (make-string header-len :element-type 'base-char)))
                   (loop for index from 0 below header-len do
                     (setf (schar buffer index) (code-char (read-byte stream))))
                   buffer))))
      (values
       (gethash "shape" dict)
       (dtype-from-code (gethash "descr" dict))
       (gethash "fortran_order" dict)
       (* 8 (+ header-len (if (= 1 major-version) 10 12)))))))

(defun load-array-metadata (filename/stream)
  (if (streamp filename/stream)
      (load-array-metadata/stream filename/stream)
      (with-open-file (stream filename/stream :direction :input :element-type 'unsigned-byte)
        (load-array-metadata/stream stream))))

(defun load-array/stream (stream)
  ;; We actually open the file twice, once to read the metadata - one byte
  ;; at a time, and once to read the array contents with a suitable element
  ;; type (e.g. (unsigned-byte 32) for single precision floating-point
  ;; numbers).
  (multiple-value-bind (dimensions dtype fortran-order header-bits)
      (load-array-metadata/stream stream)
    (declare (ignore header-bits))
    (let* ((element-type (dtype-type dtype))
           (array (make-array dimensions :element-type element-type))
           (total-size (array-total-size array)))
      (unless (not fortran-order)
        (error "Reading arrays in Fortran order is not yet supported."))
      ;; TODO Respect fortran-order
      (with-decoding ((dtype-endianness dtype))
        (etypecase array
          ((simple-array single-float)
           (loop for index below total-size do
             (setf (row-major-aref array index) (float32 stream))))
          ((simple-array double-float)
           (loop for index below total-size do
             (setf (row-major-aref array index) (float64 stream))))
          ((simple-array (complex single-float))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (complex (float32 stream) (float32 stream)))))
          ((simple-array (complex double-float))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (complex (float64 stream)
                            (float64 stream)))))
          ((simple-array (signed-byte 8))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (int8 stream))))
          ((simple-array (unsigned-byte 8))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (uint8 stream))))
          ((simple-array (unsigned-byte 16))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (uint16 stream))))
          ((simple-array (signed-byte 16))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (int16 stream))))
          ((simple-array (unsigned-byte 32))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (uint32 stream))))
          ((simple-array (signed-byte 32))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (int32 stream))))
          ((simple-array (unsigned-byte 64))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (uint64 stream))))
          ((simple-array (signed-byte 64))
           (loop for index below total-size do
             (setf (row-major-aref array index)
                   (int64 stream))))))
      array)))

(defun load-array (filename/stream)
  (if (streamp filename/stream)
      (load-array/stream filename/stream)
      (with-open-file (stream filename/stream :direction :input :element-type 'unsigned-byte)
        (load-array/stream stream))))
