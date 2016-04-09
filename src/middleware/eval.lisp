(in-package #:nrepl)

(define-condition evaluation-error (error)
  ((text :initarg :text :reader text)
   (orig :initarg :orig :reader orig)
   (data :initarg :data :reader data :initform ())))

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

(defun get-forms (code)
  "Get all lisp forms from the given hunk of code.

   Signal an evaluation-error if the input is mangled.

   "
  (handler-case
    (read-all-from-string code)
    (error (e)
           (error 'evaluation-error
                  :text "Malformed input!"
                  :orig e))))

(defun clean-backtrace (backtrace)
  (format nil "~{~A~^~%~}"
          (loop :for line :in (split-sequence:split-sequence #\newline backtrace)
                :until (ppcre:scan "NREPL::NREPL-EVALUATE-FORM" line)
                :collect line)))

(defun nrepl-evaluate-form (form)
  (declare (optimize (debug 3)))
  ;im so sorry you have to see this
  (prin1-to-string
    (handler-bind
      ((error
         (lambda (err)
           ; if we hit an error, print the backtrace to the stream before
           ; reraising.  if we wait til later to print it, it'll be too late.
           (error 'evaluation-error
                  :text "Error during evaluation!"
                  :orig err
                  :data (list
                          "form" (prin1-to-string form)
                          "backtrace" (clean-backtrace
                                        #+sbcl (with-output-to-string (s)
                                                 (sb-debug:print-backtrace
                                                   :stream s
                                                   :print-frame-source t
                                                   :from :interrupted-frame))
                                        #-sbcl "dunno"))))))
      (eval form))))

(define-middleware wrap-eval "eval" message
  (let* ((code (fset:lookup message "code"))
         (captured-out (flex:make-in-memory-output-stream))
         (captured-err (flex:make-in-memory-output-stream))
         (*standard-output*
           (flex:make-flexi-stream captured-out :external-format :utf-8))
         (*error-output*
           (flex:make-flexi-stream captured-err :external-format :utf-8)))
    (flet ((eval-form (form)
             (let ((result (nrepl-evaluate-form form)))
               (respond message
                        (make-map "form" (prin1-to-string form)
                                  "value" result))))
           (error-respond (e)
             (respond message
                      (apply #'make-map
                             "status" '("error")
                             "error" (text e)
                             "original" (format nil "~A" (orig e))
                             (data e))))
           (make-shuttle-thread (stream desc)
             (bt:make-thread
               (lambda () (shuttle-stream stream desc message))
               :name (format nil "NREPL ~A writer" desc))))
      (unwind-protect
        (progn
          (make-shuttle-thread captured-out "stdout")
          (make-shuttle-thread captured-err "stderr")
          (handler-case
            (progn
              (loop for form in (get-forms code) do (eval-form form))
              (respond message (make-map "status" '("done"))))
            (evaluation-error (e) (error-respond e))))
        (close captured-out)
        (close captured-err)))))

