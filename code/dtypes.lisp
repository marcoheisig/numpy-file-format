(cl:in-package #:numpy-file-format)

(defconstant +endianness+
  #+little-endian :little-endian
  #+big-endian  :big-endian)

(defgeneric dtype-name (dtype))

(defgeneric dtype-endianness (dtype))

(defgeneric dtype-type (dtype))

(defgeneric dtype-code (dtype))

(defgeneric dtype-size (dtype))

(defparameter *dtypes* '())

(defclass dtype ()
  ((%type :initarg :type :reader dtype-type)
   (%code :initarg :code :reader dtype-code)
   (%size :initarg :size :reader dtype-size)
   (%endianness :initarg :endianness :reader dtype-endianness)))

(defmethod print-object ((dtype dtype) stream)
  (print-unreadable-object (dtype stream :type t)
    (prin1 (dtype-code dtype) stream)))

(defun dtype-from-code (code)
  (or (find code *dtypes* :key #'dtype-code :test #'string=)
      (error "Cannot find dtype for the code ~S." code)))

(defun dtype-from-type (type)
  (or (find-if
       (lambda (dtype)
         (and (eq (dtype-endianness dtype) +endianness+)
              (subtypep type (dtype-type dtype))))
       *dtypes*)
      (error "Cannot find dtype for type ~S." type)))

(defun define-dtype (code type size &optional (endianness +endianness+))
  (let ((dtype (make-instance 'dtype
                 :code code
                 :type type
                 :size size
                 :endianness endianness)))
    (pushnew dtype *dtypes* :key #'dtype-code :test #'string=)
    dtype))

(defun define-multibyte-dtype (code type size)
  (define-dtype (concatenate 'string "<" code) type size :little-endian)
  (define-dtype (concatenate 'string ">" code) type size :big-endian)
  (define-dtype code type size +endianness+)
  (define-dtype (concatenate 'string "|" code) type size)
  (define-dtype (concatenate 'string "=" code) type size +endianness+))

(define-dtype "O" 't 64)
(define-dtype "?" 'bit 1)
(define-dtype "b" '(unsigned-byte 8) 8)
(define-multibyte-dtype "i1" '(signed-byte 8) 8)
(define-multibyte-dtype "i2" '(signed-byte 16) 16)
(define-multibyte-dtype "i4" '(signed-byte 32) 32)
(define-multibyte-dtype "i8" '(signed-byte 64) 64)
(define-multibyte-dtype "u1" '(unsigned-byte 8) 8)
(define-multibyte-dtype "u2" '(unsigned-byte 16) 16)
(define-multibyte-dtype "u4" '(unsigned-byte 32) 32)
(define-multibyte-dtype "u8" '(unsigned-byte 64) 64)
(define-multibyte-dtype "f4" 'single-float 32)
(define-multibyte-dtype "f8" 'double-float 64)
(define-multibyte-dtype "c8" '(complex single-float) 64)
(define-multibyte-dtype "c16" '(complex double-float) 128)

;; Finally, let's sort *dtypes* such that type queries always find the most
;; specific entry first.
(setf *dtypes* (stable-sort *dtypes* #'subtypep :key #'dtype-type))
