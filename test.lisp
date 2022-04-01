;;;; test.lisp
(in-package :cl-user)
(uiop:define-package :synchronized-hash-tables-test
  (:mix :synchronized-hash-tables :cl :parachute))

(in-package :synchronized-hash-tables-test)

(define-test synchronized-hash-table)

(define-test predicates
  :parent synchronized-hash-table
  (let ((hash-table (make-synchronized-hash-table)))
    (true (hash-table-p hash-table))
    (of-type hash-table hash-table)))

(define-test selectors
  :parent synchronized-hash-table
  (let ((hash-table (make-synchronized-hash-table :test 'eql :size 10 :rehash-size 2.0 :rehash-threshold 1.0)))
    (is eql 'eql (hash-table-test hash-table))
    (is = 2.0 (hash-table-rehash-size hash-table))
    (is = 1.0 (hash-table-rehash-threshold hash-table))
    (is = 0 (hash-table-count hash-table))))

(define-test crud
  :parent synchronized-hash-table
  (let ((hash-table (make-synchronized-hash-table)))
    ;; create
    (is = 1 (setf (gethash :a hash-table) 1))
    (is = 2 (setf (gethash :b hash-table) 2))
    (is = 3 (setf (gethash :c hash-table) 3))
    (is = 3 (hash-table-count hash-table))
    ;; read
    (false (gethash :d hash-table))
    (is eql :nil (gethash :d hash-table :nil))
    (is = 2 (gethash :b hash-table))
    ;; update
    (is = 22 (setf (gethash :b hash-table) 22))
    (is = 22 (gethash :b hash-table))
    ;; delete
    (true (remhash :b hash-table))
    (false (remhash :d hash-table))
    (is = 2 (hash-table-count hash-table))
    (is eq hash-table (clrhash hash-table))
    (is = 0 (hash-table-count hash-table))))

(define-test iteration
  :parent synchronized-hash-table)

(define-test maphash-iteration
  :parent iteration
  (let ((hash-table (make-synchronized-hash-table)))
    (loop for i below 10
          do (setf (gethash i hash-table) i))
    (is = 10 (hash-table-count hash-table))

    (maphash #'(lambda (key value)
                 (setf (gethash key hash-table) (* value value)))
             hash-table)
    (is = 81 (gethash 9 hash-table))
    (is = 10 (hash-table-count hash-table))

    (maphash #'(lambda (key value)
                 (when (oddp value) (remhash key hash-table)))
             hash-table)
    (is = 5 (hash-table-count hash-table))))

(define-test hashmap-iterator-iteration
  :parent iteration
  (let ((hash-table (make-synchronized-hash-table)))
    (loop for i below 10
          do (setf (gethash i hash-table) i))
    (is = 10 (hash-table-count hash-table))

    (let ((generated-entries '()))
      (with-hash-table-iterator (generator hash-table)
        (loop (multiple-value-bind (more? key value) (generator)
                (unless more? (return))
                (push (list key value) generated-entries))))
      (is = 10 (length generated-entries)))))


;;; Concurrency tests
(define-test concurrency
  :parent synchronized-hash-table)

(define-test concurrent-increase
  :parent concurrency
  (let ((hash-table (make-synchronized-hash-table))
        (task-count 100)
        (repetitions 10000))
    (setf (gethash :key hash-table) 0)
    (mapc #'bt:join-thread
          (loop repeat task-count
                collect (bt:make-thread
                         (lambda ()
                           (loop repeat repetitions
                                 do (with-locked-hash-table (hash-table)
                                      (incf (gethash :key hash-table))))))))
    (is = 1 (hash-table-count hash-table))
    (is = (* task-count repetitions) (gethash :key hash-table))))

(defvar *task-index* nil
  "A helper special variable to aid with thread bindings")

(define-test concurrent-create
  :parent concurrency
  (let ((hash-table (make-synchronized-hash-table))
        (task-count 100)
        (repetitions 10000))
    (mapc #'bt:join-thread
          (loop for task-index below task-count
                collect
                (let ((bt:*default-special-bindings*
                        (acons '*task-index* task-index bt:*default-special-bindings*)))
                  (bt:make-thread
                   (lambda ()
                     (loop for rep below repetitions
                           for key = (+ (* *task-index* repetitions) rep)
                           do (setf (gethash key hash-table) key)))))))
    (is = (* task-count repetitions) (hash-table-count hash-table))))
