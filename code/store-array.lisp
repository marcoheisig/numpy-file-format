(in-package #:numpy-file-format)

(defun array-metadata-string (array)
  (with-output-to-string (stream nil :element-type 'base-char)
    (format stream "{'descr': '~A', ~
                     'fortran_order': ~:[False~;True~], ~
                     'shape': (~{~D,~^ ~}), }"
            (dtype-code (dtype-from-type (array-element-type array)))
            nil
            (array-dimensions array))))

(defun store-array/stream (array stream &key (endianness +endianness+))
  (let* ((metadata (array-metadata-string array))
         (metadata-length (- (* 64 (ceiling (+ 10 (length metadata)) 64)) 10)))
    (write-sequence #(#x93 78 85 77 80 89) stream) ; The magic string.
    (write-byte 1 stream) ; Major version.
    (write-byte 0 stream) ; Minor version.
    ;; Write the length of the metadata string (2 bytes, little endian).
    (write-byte (ldb (byte 8 0) metadata-length) stream)
    (write-byte (ldb (byte 8 8) metadata-length) stream)
    ;; Write the metadata string.
    (loop for char across metadata do
      (write-byte (char-code char) stream))
    ;; Pad the header with spaces for 64 byte alignment.
    (loop repeat (- metadata-length (length metadata) 1) do
      (write-byte (char-code #\space) stream))
    (write-byte (char-code #\newline) stream) ; Finish with a newline.
    (let ((total-size (array-total-size array)))
      (with-encoding (endianness)
        (etypecase array
          ((simple-array single-float)
           (loop for index below total-size do
             (float32 stream (row-major-aref array index))))
          ((simple-array double-float)
           (loop for index below total-size do
             (float64 stream (row-major-aref array index))))
          ((simple-array (complex single-float))
           (loop for index below total-size do
             (let ((c (row-major-aref array index)))
               (float32 stream (realpart c))
               (float32 stream (imagpart c)))))
          ((simple-array (complex double-float))
           (loop for index below total-size do
             (let ((c (row-major-aref array index)))
               (float64 stream (the double-float (realpart c)))
               (float64 stream (the double-float (imagpart c))))))
          ((simple-array (signed-byte 8))
           (loop for index below total-size do
             (int8 stream (row-major-aref array index))))
          ((simple-array (unsigned-byte 8))
           (loop for index below total-size do
             (uint8 stream (row-major-aref array index))))
          ((simple-array (unsigned-byte 16))
           (loop for index below total-size do
             (uint16 stream (row-major-aref array index))))
          ((simple-array (signed-byte 16))
           (loop for index below total-size do
             (int16 stream (row-major-aref array index))))
          ((simple-array (unsigned-byte 32))
           (loop for index below total-size do
             (uint32 stream (row-major-aref array index))))
          ((simple-array (signed-byte 32))
           (loop for index below total-size do
             (int32 stream (row-major-aref array index))))
          ((simple-array (unsigned-byte 64))
           (loop for index below total-size do
             (uint64 stream (row-major-aref array index))))
          ((simple-array (signed-byte 64))
           (loop for index below total-size do
             (int64 stream (row-major-aref array index)))))))))

(defun store-array (array filename/stream)
  (if (streamp filename/stream)
      (store-array/stream array filename/stream :endianness :little-endian)
      (with-open-file (stream filename/stream
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (store-array/stream array stream :endianness :little-endian))))
