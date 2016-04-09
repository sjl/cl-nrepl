(in-package #:nrepl)

(defun pretty-string (form)
  "Return a prettified string version of `form`, indented nicely."
  #-ccl (format nil "~A" form)
  ;; CCL's format doesn't indent forms nicely
  #+ccl (with-output-to-string (*standard-output*)
          (pprint form)))

(define-middleware wrap-macroexpand "macroexpand" message
  ;; TODO: handle mangled input
  (let ((form (read-from-string (fset:lookup message "form"))))
    (respond message
             (make-map
               "status" '("done")
               "macroexpand" (pretty-string (macroexpand form))
               "macroexpand-1" (pretty-string (macroexpand-1 form))))))



