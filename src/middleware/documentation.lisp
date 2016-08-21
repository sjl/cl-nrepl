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


(defun parse-symbol-designator-package (string in-package)
  (let ((parts (split-sequence-if (partial #'char= #\:)
                                  (string-upcase string))))
    (case (length parts)
      (0 nil)

      ;; FOO -> ("FOO")
      (1 (parse-in-package in-package))

      ;; :FOO -> ("" "FOO")
      ;; P:FOO -> ("P" "FOO")
      (2 (if (string= (first parts) "")
           (find-package "KEYWORD")
           (parse-in-package in-package)))

      ;; P::FOO -> ("P" "" "FOO")
      (3 (find-package (first parts)))
      (t nil))))

(defun parse-symbol-designator-name (string)
  (let ((parts (split-sequence-if (partial #'char= #\:)
                                  (string-upcase string))))
    (case (length parts)
      (0 nil)

      ;; FOO -> ("FOO")
      (1 (first parts))

      ;; :FOO -> ("" "FOO")
      ;; P:FOO -> ("P" "FOO")
      (2 (second parts))

      ;; P::FOO -> ("P" "" "FOO")
      (3 (third parts))

      (t nil))))

(defun find-symbol-harder (symbol-designator &optional in-package)
  "Return the symbol object with the given `symbol-designator`.

  This should work with names like:

    FOO (uses the `in-package` designator)
    P:FOO (looks in package P)
    P::FOO (looks in package P)
    :FOO (keyword)

  "
  (let ((package (parse-symbol-designator-package symbol-designator in-package))
        (name (parse-symbol-designator-name symbol-designator)))
    (when (and name package)
      (find-symbol name package))))


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
