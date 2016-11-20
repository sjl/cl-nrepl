(in-package :nrepl)

(defun pretty-string (form)
  "Return a prettified string version of `form`, indented nicely."
  (with-output-to-string (*standard-output*)
    (let ((*print-pretty* t)
          (*print-escape* t))
      (write form))))

(define-middleware wrap-macroexpand "macroexpand" message
  ;; TODO: handle mangled input
  (let* ((*package* (parse-in-package (fset:lookup message "in-package")))
         (form (read-from-string (fset:lookup message "form"))))
    (respond message
             (make-map
               "status" '("done")
               "macroexpand" (pretty-string (macroexpand form))
               "macroexpand-1" (pretty-string (macroexpand-1 form))))))



