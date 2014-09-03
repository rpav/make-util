(defpackage :make-util
  (:use #:cl #:alexandria)
  (:export #:make-util #:util-depends))

(in-package :make-util)

 ;; Variables

(defvar *util-depends* (make-hash-table))
(defvar *all-symbols* nil)
