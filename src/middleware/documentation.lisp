(in-package #:nrepl)

(defun find-lambda-list (s)
  (when (fboundp s)
    (sb-introspect:function-lambda-list s)))

(defun find-symbol-harder (name)
  (flet ((split-string (s delim)
           (let ((idx (position delim s)))
             (if idx
               (cons (subseq s 0 idx)
                     (subseq s (1+ idx)))
               (cons nil s)))))
    (destructuring-bind (pack . symb) (split-string (string-upcase name) #\:)
      (find-symbol symb (if pack
                          (find-package pack)
                          *package*)))))

(defmiddleware wrap-documentation "documentation" message
  (let* ((s (find-symbol-harder (fset:lookup message "symbol"))))
    (respond message
             (with-when
               (make-map "status" '("done"))
               "type-docstring" (documentation s 'type)
               "structure-docstring" (documentation s 'structure)
               "variable-docstring" (documentation s 'variable)
               "setf-docstring" (documentation s 'setf)
               "function-docstring" (documentation s 'function)
               "function-arglist" (when-let ((arglist (find-lambda-list s)))
                                    (prin1-to-string arglist))))))

