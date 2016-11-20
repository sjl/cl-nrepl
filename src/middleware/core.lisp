(in-package :nrepl)

(defvar *middleware* (fset:empty-map))


(defmacro handle-op (message op fallback &rest body)
  `(if (equal ,op (fset:lookup ,message "op"))
    (progn ,@body)
    (funcall ,fallback ,message)))

(defmacro define-middleware (name op message-binding &body body)
  "Define a middleware at the symbol `name` to handle `op`.

  As the body is executing `message-binding` will be bound to the message map.

  "
  (let ((fallback (gensym)))
    `(defun ,name (,fallback)
      (lambda (,message-binding)
        (handle-op ,message-binding ,op ,fallback
                   ,@body)))))
