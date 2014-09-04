(in-package :make-util)

(defun read-function-source (fun)
  (let ((spec (swank-backend:find-source-location fun)))
    (when spec
      (let ((file (cadr (assoc :file (cdr spec))))
            (pos (cadr (assoc :position (cdr spec)))))
        (with-open-file (s file :direction :input)
          (file-position s (1- pos))
          (read s))))))

(defun read-source (symbol)
  (let ((macro (macro-function symbol))
        (fun (symbol-function symbol))
        (compiler-macro (compiler-macro-function symbol)))
    (if macro
        (read-function-source macro)
        (values (read-function-source fun)
                (and compiler-macro
                     (read-function-source compiler-macro))))))

(defun pprint-source (list &optional (stream *standard-output*))
  (let ((*print-case* :downcase)
        (*print-right-margin* 80))
    (pprint list stream)))

(defun write-header (stream filename symbols package exportp)
  (let ((package (package-name (find-package package))))
    (format stream "~
;; -*- lisp -*-
;;
;; Re-run this command to regenerate this file.  It'll overwrite
;; whatever is there, so make sure it's going to the right place:~%")
    (format stream "~&#+(or)")
    (pprint-source `(make-util ',filename
                                :package ,package
                                :symbols ',symbols
                                :exportp ,exportp)
                   stream)
    (format stream "~%
;; ===================================================================
\(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ~S)
    (make-package ~S :use '(#:cl))))
\(in-package ~S)
" package package package)
    (fresh-line stream)))

(defun maybe-warn-about (name source symbols)
  (if (listp source)
      (unless (or (member (car source)
                          '(defun defmacro define-compiler-macro))
                  (member (car source)
                          (gethash name *util-depends*))
                  (member (car source) symbols))
        (warn "Symbol ~S possibly defined by a macro that's not included (~S)"
              name (car source)))
      (warn "Source for ~S is not a list.  It may not be compiled, or the source
may have been modified." name)))

(defun write-sources (stream symbols &key exportp)
  "Write source for each symbol in SYMBOLS to STREAM"
  (loop for s in symbols
        do (unless (member s *all-symbols*)
             (push s *all-symbols*)
             (when-let (other-sources (gethash s *util-depends*))
               (write-sources stream other-sources :exportp exportp))
             (multiple-value-bind (source compiler-macro-source)
                 (read-source s)
               (maybe-warn-about s source symbols)
               (let ((actual-source
                       `(eval-when (:compile-toplevel :load-toplevel :execute)
                          (unless (fboundp ',(intern (symbol-name s)))
                            ,source
                            ,@(when (and compiler-macro-source
                                         (not (equal source compiler-macro-source)))
                                (list compiler-macro-source))))))
                 (pprint-source actual-source stream))
               (fresh-line stream)
               (when exportp
                 (pprint-source `(export ',(intern (symbol-name s))) stream)
                 (fresh-line stream))))))

(defun make-util (filename-or-asdf &key symbols (package *package*) (exportp t))
  (let ((actual-filename
          (if (listp filename-or-asdf)
              (apply #'asdf-path filename-or-asdf)
              filename-or-asdf)))
    (with-open-file (s actual-filename :direction :output :if-exists :supersede)
      (let ((*package* (find-package package)))
        (write-header s filename-or-asdf symbols package exportp)
        (let (*all-symbols*)
          (write-sources s symbols :exportp exportp))))))

(defun util-depends (util-name &rest other-symbols)
  (setf (gethash util-name *util-depends*)
        (copy-list other-symbols)))
