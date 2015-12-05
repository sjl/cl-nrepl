(in-package #:nrepl)

(defvar *middleware* (fset:empty-map))


(defmacro handle-op (message op fallback &rest body)
  `(if (equal ,op (fset:lookup ,message "op"))
     (progn ,@body)
     (funcall ,fallback ,message)))

(defmacro define-middleware (name op message-binding &rest body)
  (let ((fallback (gensym)))
  `(defun ,name (,fallback)
     (lambda (,message-binding)
       (handle-op ,message-binding ,op ,fallback
                  ,@body)))))
