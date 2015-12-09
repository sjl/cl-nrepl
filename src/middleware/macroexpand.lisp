(in-package #:nrepl)

(define-middleware wrap-macroexpand "macroexpand" message
  (let ((form (read-from-string (fset:lookup message "form"))))
    (respond message
             (make-map
               "status" '("done")
               "macroexpand" (format nil "~A" (macroexpand form))
               "macroexpand-1" (format nil "~A" (macroexpand-1 form))))))



