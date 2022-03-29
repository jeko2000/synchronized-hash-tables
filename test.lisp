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
    (is = 10 (hash-table-size hash-table))
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

;; Initialize kernel
(setf lparallel:*kernel* (lparallel:make-kernel 8))

(define-test concurrent-increase
  :parent concurrency
  (let ((hash-table (make-synchronized-hash-table))
        (channel (lparallel:make-channel))
        (task-count 100)
        (repetitions 10000))
    (setf (gethash :key hash-table) 0)
    (loop repeat task-count
          do (lparallel:submit-task channel (lambda ()
                                              (loop for rep below repetitions
                                                    do (with-locked-hash-table (hash-table)
                                                         (incf (gethash :key hash-table)))))))
    (loop repeat task-count do (lparallel:receive-result channel))
    (is = 1 (hash-table-count hash-table))
    (is = (* task-count repetitions) (gethash :key hash-table))))

(define-test concurrent-create
  :parent concurrency
  (let ((hash-table (make-synchronized-hash-table))
        (channel (lparallel:make-channel))
        (task-count 100)
        (repetitions 10000))
    (loop for task-index below task-count
          do (lparallel:submit-task channel (lambda (start)
                                              (loop for rep below repetitions
                                                    for key = (+ start rep)
                                                    do (setf (gethash key hash-table) key)))
                                    (* task-index repetitions)))
    (loop repeat task-count do (lparallel:receive-result channel))
    (is = (* task-count repetitions) (hash-table-count hash-table))))

(define-test concurrent-create-and-delete
  :parent concurrency
  (let ((hash-table (make-synchronized-hash-table))
        (channel (lparallel:make-channel))
        (create-queue (lparallel.queue:make-queue))
        (delete-queue (lparallel.queue:make-queue))
        (task-count 100)
        (repetitions 10000))
    (loop for task-index below task-count
          do (lparallel:submit-task channel (lambda (start)
                                              (lparallel.queue:pop-queue create-queue)
                                              (loop for rep below repetitions
                                                    for key = (+ start rep)
                                                    do (setf (gethash key hash-table) key)
                                                       (lparallel.queue:push-queue key delete-queue)))
                                    (* task-index repetitions)))
    (loop repeat task-count
          do (lparallel:submit-task channel (lambda ()
                                              (loop for rep below repetitions
                                                    for key = (lparallel.queue:pop-queue delete-queue)
                                                    do (remhash key hash-table)))))
    (loop repeat task-count
          do (lparallel.queue:push-queue t create-queue))
    (loop repeat (* 2 task-count) do (lparallel:receive-result channel))
    (is = 0 (hash-table-count hash-table))))
