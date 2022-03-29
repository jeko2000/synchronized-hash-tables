;;;; synchronized-hash-tables.lisp
(in-package :cl-user)
(uiop:define-package :synchronized-hash-tables
  (:use :cl)
  (:shadow #:hash-table
           #:hash-table-p
           #:gethash
           #:remhash
           #:maphash
           #:clrhash
           #:hash-table-count
           #:with-hash-table-iterator
           #:hash-table-rehash-size
           #:hash-table-rehash-threshold
           #:hash-table-size
           #:hash-table-test)
  (:export #:make-synchronized-hash-table
           #:with-locked-hash-table
           #:hash-table
           #:hash-table-p
           #:gethash
           #:remhash
           #:maphash
           #:clrhash
           #:hash-table-count
           #:with-hash-table-iterator
           #:hash-table-rehash-size
           #:hash-table-rehash-threshold
           #:hash-table-size
           #:hash-table-test))

(in-package :synchronized-hash-tables)

(defstruct (synchronized-hash-table (:constructor %make-synchronized-hash-table))
  lock
  table)

(defun make-synchronized-hash-table (&rest args &key test size rehash-size rehash-threshold &allow-other-keys)
  (declare (ignorable test size rehash-size rehash-threshold))
  (%make-synchronized-hash-table :lock (bt:make-recursive-lock)
                                 :table (apply #'make-hash-table args)))

(defmacro with-locked-hash-table ((hash-table) &body body)
  `(bt:with-recursive-lock-held ((synchronized-hash-table-lock ,hash-table))
     ,@body))

(deftype hash-table ()
  `(or cl:hash-table synchronized-hash-table))

(defun hash-table-p (object)
  (or (cl:hash-table-p object)
      (synchronized-hash-table-p object)))

(defun gethash (key hash-table &optional default)
  (with-locked-hash-table (hash-table)
    (cl:gethash key (synchronized-hash-table-table hash-table) default)))

(defun (setf gethash) (new-value key hash-table)
  (with-locked-hash-table (hash-table)
    (setf (cl:gethash key (synchronized-hash-table-table hash-table)) new-value)))

(defun remhash (key hash-table)
  (with-locked-hash-table (hash-table)
    (cl:remhash key (synchronized-hash-table-table hash-table))))

(defun maphash (function hash-table)
  (with-locked-hash-table (hash-table)
    (cl:maphash function (synchronized-hash-table-table hash-table))))

(defun clrhash (hash-table)
  (with-locked-hash-table (hash-table)
    (cl:clrhash (synchronized-hash-table-table hash-table)))
  hash-table)

(defun hash-table-count (hash-table)
  (with-locked-hash-table (hash-table)
    (cl:hash-table-count (synchronized-hash-table-table hash-table))))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  `(cl:with-hash-table-iterator (,name (synchronized-hash-table-table ,hash-table))
     ,@body))

(defun hash-table-rehash-size (hash-table)
  (cl:hash-table-rehash-size (synchronized-hash-table-table hash-table)))

(defun hash-table-rehash-threshold (hash-table)
  (cl:hash-table-rehash-threshold (synchronized-hash-table-table hash-table)))

(defun hash-table-size (hash-table)
  (cl:hash-table-size (synchronized-hash-table-table hash-table)))

(defun hash-table-test (hash-table)
  (cl:hash-table-test (synchronized-hash-table-table hash-table)))
