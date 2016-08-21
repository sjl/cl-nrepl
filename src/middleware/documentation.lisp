(in-package #:nrepl)

(defun lambda-list-to-string (l)
  "Return a single-line string of the lambda list."
  (if (listp l)
    (format nil "(~{~A~^ ~})" (mapcar #'lambda-list-to-string l))
    (princ-to-string l)))

(defun find-lambda-list (s)
  "Return the lambda list for the given symbol.

  Will return `nil` if none is found.  A second value is returned to indicate
  whether it was found.

  "
  (if (fboundp s)
    (values
      #+sbcl (sb-introspect:function-lambda-list s)
      #+ccl (ccl:arglist s)
      t)
    (values nil nil)))

(defun find-symbol-harder (name &optional in-package)
  "Return the symbol object with the given `name`.

  This should work with names like:

    FOO (assumes the current package)
    P:FOO (looks in package P)
    :FOO (keyword)

  "
  ;; TODO: add support for:
  ;;   P::FOO
  (flet ((split-string (s delim)
           (let ((idx (position delim s)))
             (if idx
               (cons (subseq s 0 idx)
                     (subseq s (1+ idx)))
               (cons nil s)))))
    (destructuring-bind (package-name . symbol-name)
        (split-string (string-upcase name) #\:)
      (let ((package
              (cond
                ((null package-name) (parse-in-package in-package)) ; no : at all
                ((string= "" package-name) (find-package "KEYWORD")) ; :keyw
                (t (find-package package-name))))) ; pack:sym
        (when package
          (find-symbol symbol-name package))))))


(define-middleware wrap-documentation "documentation" message
  (let ((s (find-symbol-harder (fset:lookup message "symbol")
                               (fset:lookup message "in-package"))))
    (respond message
             (with-when
                 (make-map "status" '("done"))
               "type-docstring" (documentation s 'type)
               "structure-docstring" (documentation s 'structure)
               "variable-docstring" (documentation s 'variable)
               "setf-docstring" (documentation s 'setf)
               "function-docstring" (documentation s 'function)
               "function-arglist"
               (multiple-value-bind (arglist foundp)
                   (find-lambda-list s)
                 (when foundp
                   (princ-to-string (cons s arglist))))))))

(define-middleware wrap-arglist "arglist" message
  (let ((s (find-symbol-harder (fset:lookup message "symbol")
                               (fset:lookup message "in-package"))))
    (respond message
             (with-when
               (make-map "status" '("done"))
               "function-arglist"
               (multiple-value-bind (arglist foundp)
                   (find-lambda-list s)
                 (when foundp
                   (lambda-list-to-string (cons s arglist))))))))
