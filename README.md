## Introduction

SYNCHRONIZED-HASH-TABLES is a small library exposing a unified interface for the creation and manipulation of *synchronized* (aka concurrent) hash tables.

## Prerequisites
These are the prerequisites to best follow this README.

1. [Hash tables](http://cl-cookbook.sourceforge.net/hashes.html)
2. [Threads/Concurrency](https://lispcookbook.github.io/cl-cookbook/process.html)
   + Specifically, you should be familiar with typical problems arising from concurrent mutation on shared resources.
3. [Quicklisp](https://www.quicklisp.org/beta/)
   + You should have a [Quicklisp installed](https://www.quicklisp.org/beta/#installation) and read through its [documentation](https://www.quicklisp.org/beta/).

## Definitions
A *synchronized hash table* is a hash table that can accommodate multiple *concurrent* accesses.

## Motivation
Though the Common Lisp standard does not mention concurrency, several CL implementations offer varying support for concurrency constructs such as [threads](https://en.wikipedia.org/wiki/Thread_(computing)), [semaphores](https://en.wikipedia.org/wiki/Semaphore_(programming)), [condition variables](https://en.wikipedia.org/wiki/Monitor_(synchronization)#Condition_variables), [locks](https://en.wikipedia.org/wiki/Lock_(computer_science)), timers, and atomic operations. Unfortunately, implementations often use different names and/or packages for the same underlying constructs which  presents an inconvenience to a developer writing an application or library aimed to be portable across multiple implementations. To solve this, most developers reach for the [bordeax-threads](https://sionescu.github.io/bordeaux-threads/) compatibility library which irons over these minor differences among implementations and exposes a unified, portable interface to the various concurrency primitives listed above.

SYNCHRONIZED-HASH-TABLES takes inspiration from [bordeax-threads](https://sionescu.github.io/bordeaux-threads/) and aims to provide a unified, portable interface for the creation and manipulation of synchronized hash tables.

## Overview
SYNCHRONIZED-HASH-TABLES is one of two things depending on whether the target Common Lisp implementation already supports synchronized hash tables.

1. In cases where the target implementation already supports synchronized hash tables (e.g., [SBCL](http://www.sbcl.org), [Lispworks](http://www.lispworks.com)), SYNCHRONIZED-HASH-TABLES is little more than a thin wrapper on the existing machinery aimed to iron out the differences among supporting implementations.

2. Alternatively, SYNCHRONIZED-HASH-TABLES exposes a functional, yet somewhat slow implementation by leveraging traditional hash tables in combination with a [recursive lock](https://sionescu.github.io/bordeaux-threads/locks/recursive-lock/) (via [bordeax-threads](https://sionescu.github.io/bordeaux-threads/)).

## Usage

First, create a synchronized hash table instance via the exported `make-synchronized-hash-table` function.
The function accepts the same arguments `make-hash-table`.

```common-lisp
;;;; demo.lisp
(in-package :cl-user)
(uiop:define-package :demo
  (:use :cl)
  (:local-nicknames (:sync-ht :synchronized-hash-tables)))

(in-package :demo)

(defvar *my-sync-ht* (sync-ht:make-synchronized-hash-table :test 'eql))
```

Next, interact with the instance using the following functions/macros exported by the library.

* `clrhash`
* `gethash`
* `hash-table`
* `hash-table-count`
* `hash-table-p`
* `hash-table-rehash-size`
* `hash-table-rehash-threshold`
* `hash-table-size`
* `hash-table-test`
* `maphash`
* `remhash`
* `with-hash-table-iterator`

```common-lisp
;;;; demo.lisp
(in-package :cl-user)
(uiop:define-package :demo
  (:use :cl)
  (:local-nicknames (:sync-ht :synchronized-hash-tables)))

(in-package :demo)

(defvar *my-sync-ht* (sync-ht:make-synchronized-hash-table :test 'eql))

(defun concurrent-insert-demo ()
  (sync-ht:clrhash *my-sync-ht*)
  (let ((thread-count 100)
        (repetitions 1000))
    (setf (gethash :key *my-sync-ht*) 0)
    (mapc #'bt:join-thread
          (loop repeat thread-count
                collect (bt:make-thread
                         (lambda ()
                           (loop repeat repetitions
                                 do (sync-ht:with-locked-hash-table (*my-sync-ht*)
                                      (incf (sync-ht:gethash :key *my-sync-ht*))))))))
    (assert (= 100000 (sync-ht:gethash :key *my-sync-ht*)))
    t))

(concurrent-insert-demo)
```

## Installation
Navigate to ~/quicklisp/local-projects (or equivalent) and clone the repository as follows:
```sh
git clone https://github.com/jeko2000/synchronized-hash-tables.git
```

Next, start CL and load the library via Quicklisp:
```common-lisp
(ql:quickload "synchronized-hash-tables")
```
Enjoy!
