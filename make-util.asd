(defpackage :make-util.asdf
  (:use #:cl #:asdf))

(in-package :make-util.asdf)

(defsystem :make-util
  :description "Trivially generate a util.lisp from arbitrary local functions"
  :author "Ryan Pavlik"
  :license "BSD"
  :version "0.0"

  :depends-on (:alexandria :swank)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "util")
   (:file "make-util")))

(defmethod perform :after ((op load-op) (c (eql (find-system :make-util))))
  (pushnew :make-util *features*))
