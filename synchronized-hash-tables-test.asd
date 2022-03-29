;;;; synchronized-hash-tables-test.asd

(asdf:defsystem #:synchronized-hash-tables-test
  :description "A test system for synchronized-hash-tables."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:synchronized-hash-tables :parachute :lparallel)
  :components ((:file "test"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :synchronized-hash-tables-test)))
