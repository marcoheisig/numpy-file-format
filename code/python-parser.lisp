(cl:in-package #:numpy-file-format)

;;; This parser is not very sophisticated, but it gets the job done.

(defun read-python-object (stream &optional (skip #\,) (stop nil))
  (loop for c = (read-char stream) do
    (case c
      ((#\space #\tab) (values))
      ((#\' #\") (return (read-python-string c stream)))
      (#\( (return (read-python-tuple stream)))
      (#\[ (return (read-python-list stream)))
      (#\{ (return (read-python-dict stream)))
      ((#\T #\F)
       (unread-char c stream)
       (return (read-python-boolean stream)))
      (otherwise
       (cond ((eql c skip)
              (return (read-python-object stream nil stop)))
             ((eql c stop)
              (return stop))
             ((digit-char-p c)
              (unread-char c stream)
              (return (read-python-integer stream)))
             (t
              (error "Invalid character: ~S" c)))))))

(defun read-python-string (delimiter stream)
  (coerce
   (loop for c = (read-char stream)
         while (char/= c delimiter)
         collect c)
   'string))

(defun read-python-integer (stream)
  (let ((result 0))
    (loop for c = (read-char stream) do
      (let ((weight (digit-char-p c)))
        (if (null weight)
            (progn
              (unread-char c stream)
              (loop-finish))
            (setf result (+ (* result 10) weight)))))
    result))

(defun read-python-boolean (stream)
  (flet ((skip (string)
           (loop for c across string do
             (assert (char= (read-char stream) c)))))
    (ecase (read-char stream)
      (#\T (skip "rue") t)
      (#\F (skip "alse") nil))))

(defun read-python-tuple (stream)
  (loop for object = (read-python-object stream nil #\))
        then (read-python-object stream #\, #\))
        until (eql object #\))
        collect object))

(defun read-python-list (stream)
  (coerce
   (loop for object = (read-python-object stream nil #\])
           then (read-python-object stream #\, #\])
         until (eql object #\])
         collect object)
   'vector))

(defun read-python-dict (stream)
  (let ((dict (make-hash-table :test #'equal)))
    (loop
      (let ((key (read-python-object stream #\, #\})))
        (when (eql key #\})
          (return dict))
        (setf (gethash key dict)
              (read-python-object stream #\:))))))

(defun read-python-object-from-string (string)
  (with-input-from-string (stream string)
    (read-python-object stream)))
