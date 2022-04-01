;;;; synchronized-hash-tables.asd

(asdf:defsystem #:synchronized-hash-tables
  :description "A thin portability layer for synchronized hash tables."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "MIT"
  :version "0.1.0"
  :depends-on (:bordeaux-threads)
  :components ((:file "synchronized-hash-tables"))
  :in-order-to ((asdf:test-op (asdf:test-op :synchronized-hash-tables-test))))
