;; -*- lisp -*-
;;
;; Re-run this command to regenerate this file.  It'll overwrite
;; whatever is there, so make sure it's going to the right place:
#+(or)
(make-util '(make-util "util")
           :package
           "MAKE-UTIL"
           :symbols
           '(laconic:asdf-path)
           :exportp
           nil)

;; ===================================================================
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "MAKE-UTIL")
    (make-package "MAKE-UTIL" :use nil)))
(in-package "MAKE-UTIL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'asdf-path)
    (defun asdf-path (system &rest path)
      (asdf/component:component-pathname
        (or (asdf/find-component:find-component
              (asdf/system:find-system system t)
              path)
            (error "System ~S path not found: ~S" system path))))))
