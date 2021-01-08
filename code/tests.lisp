(defpackage :numpy-file-format/tests
  (:use :cl :numpy-file-format)
  (:export :run))

(in-package :numpy-file-format/tests)

(defparameter *array-element-types*
  `(single-float
    double-float
    (complex single-float)
    (complex double-float)
    ,@(loop for bytes in '(8 16 32 64)
            collect `(unsigned-byte ,bytes)
            collect `(signed-byte ,bytes))))

(defparameter *array-dimensions*
  '((2 3 4 5)
    (3 3 3)
    (3 3)
    (3)
    10))

(defun type= (type1 type2)
  (or (equal type1 type2)
      (values
       (and (subtypep type1 type2)
            (subtypep type2 type1)))))

(defun array= (a b)
  (and (type= (array-element-type a)
              (array-element-type b))
       (equal (array-dimensions a)
              (array-dimensions b))
       (loop for index below (array-total-size a)
             always (= (row-major-aref a index)
                       (row-major-aref b index)))))

(defun make-random-array (dimensions element-type)
  (let ((array (make-array dimensions :element-type element-type)))
    (loop for index below (array-total-size array) do
      (setf (row-major-aref array index)
            (coerce (random 2) element-type)))
    array))

(defun run ()
  (loop for element-type in *array-element-types* do
    (loop for array-dimensions in *array-dimensions* do
      (let ((array (make-random-array array-dimensions element-type)))
        (uiop:with-temporary-file (:pathname file)
          (store-array array file)
          (assert (array= array (load-array file))))))))
