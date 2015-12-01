(in-package #:nrepl)

;;; Eval
(defclass evaluator ()
  ((standard-input :initarg :in :reader in)
   (standard-output :initarg :out :reader out)
   (standard-error :initarg :err :reader err)))


(defun shuttle-stream (from-stream stream-name message)
  (do ((data "" (flex:octets-to-string
                  (flex:get-output-stream-sequence from-stream)
                  :external-format :utf-8)))
    ((and (not (open-stream-p from-stream))
          (equal data ""))
     nil)
    (when (not (equal data ""))
      (respond message (make-map "status" '("ok")
                                 stream-name data)))
    (sleep 0.1)))

(defun wrap-eval (h)
  (lambda (message)
    (handle-op
      message "eval" h
      (let* ((code (fset:lookup message "code"))
             (captured-out (flex:make-in-memory-output-stream))
             (captured-err (flex:make-in-memory-output-stream))
             (*standard-output*
               (flex:make-flexi-stream captured-out :external-format :utf-8))
             (*error-output*
               (flex:make-flexi-stream captured-err :external-format :utf-8)))
        (unwind-protect
          (progn
            (bt:make-thread
              (lambda () (shuttle-stream captured-out "stdout" message))
              :name "NREPL stdout writer")
            (bt:make-thread
              (lambda () (shuttle-stream captured-err "stderr" message))
              :name "NREPL stderr writer")
            (loop for form in (read-all-from-string code)
                  do (let ((result (prin1-to-string (eval form))))
                       (respond message
                                (make-map "form" (prin1-to-string form)
                                          "value" result))))
            (respond message (make-map "status" '("done"))))
          (close captured-out)
          (close captured-err))))))

