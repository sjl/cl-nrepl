; (in-package :nrepl)

(ql:quickload "bencode")
(ql:quickload "usocket")
(ql:quickload "flexi-streams")
(ql:quickload "bordeaux-threads")


;;;; Utilities ----------------------------------------------------------------
(defun make-hash (&rest keyvals)
  (do ((h (make-hash-table :test #'equal))
       (kvs keyvals (cddr kvs)))
    ((not kvs) h)
    (setf (gethash (first kvs) h) (second kvs))))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
                using (hash-value value)
                collect (list key value))))


;;;; Sockets ------------------------------------------------------------------
(defvar *server-thread* nil)
(defvar *socket* nil)

(defun get-stream (sock)
  "Make a flexi stream of the kind bencode wants from the socket."
  (flex:make-flexi-stream
    (usocket:socket-stream sock)
    :external-format :utf-8))


(defun accept-connections (server-socket)
  "Accept connections to the server and spawn threads to handle each."
  (loop
    (format t "Waiting for a connection...~%")
    (let ((client-socket (usocket:socket-accept server-socket
                                                :element-type '(unsigned-byte 8))))
      (format t "Connection received.  Spinning up handler thread...~%")
      (bt:make-thread
        (lambda ()
          (unwind-protect
            (let ((*socket* client-socket))
              (handler))
            (format t "Closing client connection...~%")
            (usocket:socket-close client-socket)))
        :name "NREPL Connection Handler"))))

(defun start-server (address port)
  "Fire up a server thread that will listen for connections."
  (format t "Starting server...~%")
  (let ((socket (usocket:socket-listen address port :reuse-address t)))
    (build-handler)
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


(defun write-object (o)
  "Write an object (bencoded) to *socket*."
  (bencode:encode o (get-stream *socket*))
  (force-output (get-stream *socket*)))

(defun read-object ()
  "Read an object (and bdecode it) from *socket*."
  (bencode:decode (get-stream *socket*)))


;;;; NREPL --------------------------------------------------------------------
(defun respond (message response)
  (funcall (gethash "transport" message) response))

(defmacro handle-op (message op fallback &rest body)
  `(if (equal ,op (gethash "op" ,message))
     (progn ,@body)
     (funcall ,fallback ,message)))

(defun handle-base (message)
  (respond message (make-hash "status" "unknown-op")))

(defun wrap-time (h)
  (lambda (message)
    (format t "In wrap-time...~%")
    (handle-op
      message "time?" h
      (respond message (make-hash "status" "done"
                                  "time" (get-universal-time))))))

(defun wrap-eval (h)
  (lambda (message)
    (format t "In wrap-eval...~%")
    (handle-op
      message "eval" h
      (let ((code (gethash "code" message)))
        (respond message
                 (make-hash "status" "done"
                            "result" (eval (read-from-string code))))))))

(defparameter *middleware*
  (list
    #'wrap-eval
    #'wrap-time))

(defun build-handler (base middleware)
  (if middleware
    (funcall (car middleware)
             (build-handler base (cdr middleware)))
    base))

(defun handle (message)
  (format t "Handling message:~%~A~%~%" message)
  (funcall (build-handler #'handle-base *middleware*) message))

(defun handle-message ()
  (let ((message (read-object)))
    (setf (gethash "transport" message) #'write-object)
    (handle message)))

(defun handler ()
  (loop (handle-message)))


;;;; Scratch ------------------------------------------------------------------
; (connect)
; (handle-message)
; (start-server "localhost" 8675)
; (stop-server)

; > (first (list 1 2))
; Message:
; 'd2:ns4:user7:session36:37b0fdb1-5d6d-4646-aa68-22af41e172bb5:value1:1e'
; {'ns': 'user', 'session': '37b0fdb1-5d6d-4646-aa68-22af41e172bb', 'value': '1'}
; >
; Message:
; 'd7:session36:37b0fdb1-5d6d-4646-aa68-22af41e172bb6:statusl4:doneee'
; {'session': '37b0fdb1-5d6d-4646-aa68-22af41e172bb', 'status': ['done']}

; TODO
; * Implement middleware metadata
; * Implement middleware linearization
; * Implement sessions
; * Implement Fireplace workarounds
;
;   * Look for what ops fireplace needs
;   * Look into how fireplace handles clojure namespaces
;   * Implement a minimal amount of fireplace ops (eval, reload, doc)
;   * Implement a minimal amount of fireplace workarounds
;
; * Implement other nrepl default ops
