(in-package :nrepl)

(define-middleware wrap-load-file "load-file" message
  (let ((path (fset:lookup message "path")))
    (evaluate-forms message (list `(load ,path)))))
