; (in-package :nrepl)

(ql:quickload "bencode")
(ql:quickload "usocket")
(ql:quickload "flexi-streams")
(ql:quickload "bordeaux-threads")
(ql:quickload "uuid")

(require 'sb-introspect)


;;;; Variables ----------------------------------------------------------------
(defvar *server-thread* nil)


;;;; Utilities ----------------------------------------------------------------
(defun make-hash (&rest keyvals)
  (do ((h (make-hash-table :test #'equal))
       (kvs keyvals (cddr kvs)))
    ((not kvs) h)
    (setf (gethash (first kvs) h) (second kvs))))

(defmacro when-let (bindings &rest body)
  (labels ((build (bindings body)
             (if (not bindings)
               body
               `(let ((,(caar bindings) ,(cadar bindings)))
                  (when ,(caar bindings)
                    ,(build (cdr bindings) body))))))
    (build bindings `(progn ,@body))))

(defun set-when (h &rest keyvals)
  (loop for (key val) on keyvals by #'cddr
        do (when val (setf (gethash key h) val))))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
                using (hash-value value)
                collect (list key value))))

(defun read-all-from-string (s)
  (labels ((read-next-from-string (s results)
             (if (equal (string-trim " " s) "")
               results
               (multiple-value-bind (i pos) (read-from-string s)
                 (read-next-from-string (subseq s pos) (cons i results))))))
    (nreverse (read-next-from-string s ()))))

(defmacro comment (&rest body)
  (declare (ignore body))
  nil)

(defun curry (fn &rest curried-args)
  (lambda (&rest args)
    (apply fn (append curried-args args))))

(defun random-uuid ()
  (format nil "~a" (uuid:make-v4-uuid)))

(defun hash-keys (h)
  (loop for key being the hash-keys of h
        collect key))


;;;; Sockets ------------------------------------------------------------------
(defun get-stream (sock)
  "Make a flexi stream of the kind bencode wants from the socket."
  (flex:make-flexi-stream
    (usocket:socket-stream sock)
    :external-format :utf-8))


(defun write-object (socket lock o)
  "Write an object O (bencoded) to SOCKET while holding LOCK."
  (bt:with-lock-held (lock)
                     (bencode:encode o (get-stream socket))
                     (force-output (get-stream socket))))

(defun read-object (socket)
  "Read an object (and bdecode it) from *socket*."
  (bencode:decode (get-stream socket)))


;;;; NREPL --------------------------------------------------------------------
;;; Utils
(defun respond (message response)
  (set-when response "id" (gethash "id" message))
  (funcall (gethash "transport" message) response))

(defmacro handle-op (message op fallback &rest body)
  `(if (equal ,op (gethash "op" ,message))
     (progn ,@body)
     (funcall ,fallback ,message)))


;;; Sessions
(defvar *sessions* (make-hash-table :test #'equal))

(defun create-session! (id)
  (setf (gethash id *sessions*)
        (make-hash-table :test #'equal)))

(defun get-session (id)
  (gethash id *sessions*))

(defun remove-session! (id)
  (remhash id *sessions*))

(defun get-sessions ()
  (hash-keys *sessions*))

(defun wrap-session-ls (h)
  (lambda (message)
    (handle-op
      message "ls-sessions" h
      (respond message
               (make-hash "status" "done"
                          "session" (get-sessions))))))

(defun wrap-session-close (h)
  (lambda (message)
    (handle-op
      message "close" h
      (remove-session! (gethash "session" message))
      (respond message
               (make-hash "status" "session-closed")))))


;;; Eval
(defclass evaluator ()
  ((standard-input :initarg :in :reader in)
   (standard-output :initarg :out :reader out)
   (standard-error :initarg :err :reader err)))

(defun handle-base (message)
  (respond message (make-hash "status" "unknown-op")))


(defun shuttle-stream (from-stream stream-name message)
  (do ((data "" (flex:octets-to-string
                  (flex:get-output-stream-sequence from-stream)
                  :external-format :utf-8)))
    ((and (not (open-stream-p from-stream))
          (equal data ""))
     nil)
    (when (not (equal data ""))
      (respond message (make-hash "status" "ok"
                                  stream-name data)))
    (sleep 0.1)))

(defun wrap-eval (h)
  (lambda (message)
    (handle-op
      message "eval" h
      (let* ((code (gethash "code" message))
             (captured-out (flex:make-in-memory-output-stream))
             (captured-err (flex:make-in-memory-output-stream))
             (*standard-output*
               (flex:make-flexi-stream captured-out :external-format :utf-8))
             (*error-output*
               (flex:make-flexi-stream captured-err :external-format :utf-8))
             (evaluator (make-instance 'evaluator
                                       :in nil
                                       :out captured-out
                                       :err captured-err)))
        (declare (ignore evaluator))
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
                                (make-hash "status" "done"
                                           "form" (prin1-to-string form)
                                           "value" result)))))
          (close captured-out)
          (close captured-err))))))


(defun find-lambda-list (s)
  (when (fboundp s)
    (sb-introspect:function-lambda-list s)))

(defun find-symbol-harder (name)
  (flet ((split-string (s delim)
           (let ((idx (position delim s)))
             (if idx
               (cons (subseq s 0 idx)
                     (subseq s (1+ idx)))
               (cons nil s)))))
    (destructuring-bind (pack . symb) (split-string (string-upcase name) #\:)
      (find-symbol symb (if pack
                          (find-package pack)
                          *package*)))))

(defun wrap-documentation (h)
  (lambda (message)
    (handle-op
      message "documentation" h
      (let* ((s (find-symbol-harder (gethash "symbol" message)))
             (resp (make-hash "status" "done")))
        (set-when resp
                  "type-docstring" (documentation s 'type)
                  "structure-docstring" (documentation s 'structure)
                  "variable-docstring" (documentation s 'variable)
                  "setf-docstring" (documentation s 'setf)
                  "function-docstring" (documentation s 'function)
                  "function-arglist" (when-let ((arglist (find-lambda-list s)))
                                       (prin1-to-string arglist)))
        (respond message resp)))))


;;; Plumbing
(defun middleware ()
  "Return the stack of middleware.

   In the future we should make this less horrifyingly inefficient, but for
   NREPL development its_fine.

   "
  (list
    #'wrap-session-ls
    #'wrap-session-close
    #'wrap-eval
    #'wrap-documentation))

(defun build-handler (base middleware)
  "Collapse the stack of middleware into a single handler function."
  (if middleware
    (funcall (car middleware)
             (build-handler base (cdr middleware)))
    base))

(defun handle (message)
  "Handle the given NREPL message."
  (format t "Handling message:~%~A~%~%" message)
  (funcall (build-handler #'handle-base (middleware)) message))

(defun handle-message (socket lock)
  "Read a single message from the socket and handle it."
  (let ((message (read-object socket)))
    (setf (gethash "transport" message)
          (curry #'write-object socket lock))
    (handle message)))

(defun handler (socket lock)
  "Read a series of messages from the socket, handling each."
  (loop (handle-message socket lock)))


;;;; Server -------------------------------------------------------------------
(defun accept-connections (server-socket)
  "Accept connections to the server and spawn threads to handle each."
  (loop
    (let ((client-socket (usocket:socket-accept
                           server-socket
                           :element-type '(unsigned-byte 8)))
          (write-lock (bt:make-lock "NREPL client writing lock")))
      (bt:make-thread
        (lambda ()
          (unwind-protect (handler client-socket write-lock)
            (usocket:socket-close client-socket)))
        :name "NREPL Connection Handler"))))

(defun start-server (address port)
  "Fire up a server thread that will listen for connections."
  (format t "Starting server...~%")
  (let ((socket (usocket:socket-listen address port :reuse-address t)))
    (setf *server-thread*
          (bt:make-thread
            (lambda ()
              (unwind-protect
                (accept-connections socket)
                (format t "Closing server socket...~%")
                (usocket:socket-close socket)))
            :name (format nil "NREPL Server (~a/~a)" address port)))))

(defun stop-server ()
  "Kill the server thread, if it exists."
  (let ((s (shiftf *server-thread* nil)))
    (when s
      (bt:destroy-thread s))))


;;;; Scratch ------------------------------------------------------------------
(comment
  (connect)
  (handle-message)
  (start-server "localhost" 8675)
  (stop-server))


; TODO
; * Implement middleware metadata
; * Implement middleware linearization
; * Implement sessions
; * Implement Fireplace workarounds
;   * Look for what ops fireplace needs
;   * Look into how fireplace handles clojure namespaces
;   * Implement a minimal amount of fireplace ops (eval, reload, doc)
;   * Implement a minimal amount of fireplace workarounds
; * Implement other nrepl default ops
; * Check that we're not leaking threads
; * Check that we're not leaking objects
(comment
  (defparameter s (flex:make-in-memory-output-stream))
  (defparameter fs (flex:make-flexi-stream s :external-format :utf-8))
  (close s)
  (close fs)
  (format fs "")
  (format s "Hello, world!~%")
  (open-stream-p s)
  (open-stream-p fs)
  (flex:octets-to-string (flex:get-output-stream-sequence s) :external-format :utf-8))

