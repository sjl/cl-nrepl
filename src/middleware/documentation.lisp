(in-package #:nrepl)

(defun lambda-list-to-string (lambda-list)
  "Return a single-line string of the lambda list."
  (if (listp lambda-list)
    (format nil "(~{~A~^ ~})" (mapcar #'lambda-list-to-string lambda-list))
    (princ-to-string lambda-list)))

(defun find-lambda-list (symbol)
  "Return the lambda list for the given symbol.

  Will return `nil` if none is found.  A second value is returned to indicate
  whether it was found.

  "
  (if (fboundp symbol)
    (let ((arglist (trivial-arguments:arglist symbol)))
      (if (eql :unknown arglist)
        (values nil nil)
        (values arglist t)))
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
           (first parts)))

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
  (let ((symbol (find-symbol-harder (fset:lookup message "symbol")
                                    (fset:lookup message "in-package"))))
    (respond message
             (with-when
                 (make-map "status" '("done"))
               "type-docstring" (documentation symbol 'type)
               "structure-docstring" (documentation symbol 'structure)
               "variable-docstring" (documentation symbol 'variable)
               "setf-docstring" (documentation symbol 'setf)
               "function-docstring" (documentation symbol 'function)
               "function-arglist"
               (when-found arglist (find-lambda-list symbol)
                 (princ-to-string (cons symbol arglist)))))))

(define-middleware wrap-arglist "arglist" message
  (let ((symbol (find-symbol-harder (fset:lookup message "symbol")
                                    (fset:lookup message "in-package"))))
    (respond message
             (with-when
                 (make-map "status" '("done"))
               "function-arglist"
               (when-found arglist (find-lambda-list symbol)
                 (lambda-list-to-string (cons symbol arglist)))))))
