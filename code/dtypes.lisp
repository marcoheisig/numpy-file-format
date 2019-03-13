(cl:in-package #:numpy-file-format)

(defun dtype-bytes/element (dtype)
  (let ((start (position-if #'digit-char-p dtype)))
    (if (not start)
        nil
        (parse-integer (subseq dtype start)))))

(defun parse-dtype (dtype)
  (check-type dtype string)
  (let ((bytes/element (dtype-bytes/element dtype)))
    (values
     (case (find-if #'alpha-char-p dtype)
       (#\b 'boolean)
       (#\i `(signed-byte ,(* 8 bytes/element)))
       (#\u `(unsigned-byte ,(* 8 bytes/element)))
       (#\f (ecase bytes/element
              (4 'single-float)
              (8 'double-float)))
       (otherwise 't))
     (case (schar dtype 0)
       (#\< :little-endian)
       (#\> :big-endian)
       (otherwise
        (warn "Endianness not clear.")
        :little-endian)))))
