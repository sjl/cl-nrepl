(in-package #:nrepl)

(defvar *last-trace* nil)

(define-condition evaluation-error (error)
  ((text :initarg :text :reader text)
   (orig :initarg :orig :reader orig)
   (data :initarg :data :reader data :initform ())))

(defclass evaluator ()
  ((standard-input :initarg :in :reader in)
   (standard-output :initarg :out :reader out)
   (standard-error :initarg :err :reader err)))


(defun read-data (stream)
  (flex:octets-to-string
    (flex:get-output-stream-sequence stream)
    :external-format :utf-8))

(defun shuttle-stream (stream stream-name message)
  "Read data from `stream` and shuttle it back to the client.

  Chunks of data will be read from `stream` until it's finished and closed.

  For each hunk of data read, a response will be sent back to the client using
  the transport defined in `message`, looking something like:

    {\"status\" \"ok\"
     \"stdout\" \"...data...\"}

  `stream-name` should be the name of the stream being shuttled, like
  \"stderr\", and will be used as the key in the response.

  "
  (loop
    :for data = (read-data stream)
    :until (and (not (open-stream-p stream))
                (equal data ""))
    :do (progn
          (when (not (string= data ""))
            (respond message (make-map "status" '("ok")
                                       stream-name data)))
          (sleep 0.1))))


(defun get-forms (code)
  "Get all lisp forms from `code`.

  If `code` is a string, the forms will be read out of it, and an
  `evaluation-error` signaled if the input is mangled.

  If `code` is anything else it will just be returned as-is.

  "
  (if (stringp code)
    (handler-case
        (read-all-from-string code)
      (error (e)
             (error 'evaluation-error
                    :text "Malformed input!"
                    :orig e)))
    code))


(defun parse-frame (frame)
  (list* (dissect:call frame) (dissect:args frame)))

(defun parse-stack (stack)
  (mapcar #'parse-frame (nthcdr 1 (reverse stack))))

(defun string-trace (stack)
  (format nil "~{~S~^~%~}" (parse-stack stack)))

(defun nrepl-evaluate-form (form)
  (declare (optimize (debug 3)))
  (prin1-to-string
    (handler-bind
      ((error
         (lambda (err)
           ; if we hit an error, get the stack trace before reraising.  if we
           ; wait til later to print it, it'll be too late.
           (error 'evaluation-error
                  :text "Error during evaluation!"
                  :orig err
                  :data (let ((trace (dissect:stack)))
                          (setf *last-trace* trace)
                          (list
                            "form" (prin1-to-string form)
                            "backtrace" (string-trace trace)))))))
      (dissect:with-truncated-stack ()
        (eval form)))))


(defun evaluate-forms (message forms &optional in-package)
  "Evaluate each form in `forms` and shuttle back the responses.

  `forms` can be a string, in which case the forms will be read out of it, or
  a ready-to-go list of actual forms.

  `in-package` can be a package designator, or `nil` to just use `*package*`.

  "
  (let* ((captured-out (flex:make-in-memory-output-stream))
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
                             "original" (format nil "~S" (orig e))
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
                  (let ((*package* (parse-in-package in-package)))
                    (mapc #'eval-form (get-forms forms)))
                  (respond message (make-map "status" '("done"))))
              (evaluation-error (e) (error-respond e))))
        (close captured-out)
        (close captured-err)))))
