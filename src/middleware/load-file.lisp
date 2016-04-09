(in-package #:nrepl)

(define-middleware wrap-load-file "load-file" message
  (let ((path (fset:lookup message "path")))
    (load path)
    (respond message (make-map "status" '("done")
                               "value" "T"))))
