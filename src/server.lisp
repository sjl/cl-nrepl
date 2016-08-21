(in-package #:nrepl)

;;;; Config
(defvar *verbose-debug-output* nil)
(defvar *unthreaded* nil)


;;;; Plumbing
(defun handle-base (message)
  (respond message (make-map "status" '("unknown-op"))))

(defun middleware ()
  "Return the stack of middleware.

   In the future we should make this less horrifyingly inefficient, but for
   NREPL development its_fine.

   "
  (list
    #'wrap-session
    #'wrap-session-ls
    #'wrap-session-clone
    #'wrap-session-close
    #'wrap-describe
    #'wrap-load-file
    #'wrap-macroexpand
    #'wrap-eval
    #'wrap-documentation
    #'wrap-arglist
    ))

(defun build-handler (base middleware)
  "Collapse the stack of middleware into a single handler function."
  (if middleware
    (funcall (car middleware)
             (build-handler base (cdr middleware)))
    base))

(defun handle (message)
  "Handle the given NREPL message."
  (when *verbose-debug-output*
    (log-message "Handling message: vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv~%")
    (log-message "~A~%" message)
    (log-message "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~%"))
  (funcall (build-handler #'handle-base (middleware)) message))

(defun handle-message (socket-stream lock)
  "Read and handle a single message from the socket."
  (let ((message (fset:with (read-object socket-stream)
                   "transport" (partial #'write-object socket-stream lock))))
    (handle message)))

(defun handler (socket-stream lock)
  "Read a series of messages from the socket-stream, handling each."
  (log-message "Client connected...")
  (handler-case (loop (handle-message socket-stream lock))
    (end-of-file () nil))
  (log-message "Client disconnected..."))


;;;; Server
(defvar *server-thread* nil)

(defmacro run-in-thread (thread-name &rest body)
  "Run `body` in a thread called `name` (usually).  Return the thread.

  If `nrepl::*unthreaded*` is true, the body will be executed immediately in the
  current thread and `nil` will be returned instead.  Useful for debugging.

  "
  `(let ((thunk (lambda () ,@body)))
    (if *unthreaded*
      (progn (funcall thunk) nil)
      (bt:make-thread thunk :name ,thread-name))))


(defun accept-connections (server-socket)
  "Accept connections to the server and spawn threads to handle each."
  (format t "Accepting connections...~%")
  (loop
    (let* ((client-socket (usocket:socket-accept
                            server-socket
                            :element-type '(unsigned-byte 8)))
           (socket-stream (flex:make-flexi-stream
                            (usocket:socket-stream client-socket)
                            :external-format :utf-8))
           (write-lock (bt:make-lock "NREPL client writing lock")))
      (run-in-thread "NREPL Connection Handler"
        (unwind-protect
            (handler socket-stream write-lock)
          (usocket:socket-close client-socket))))))

(defun start-server (&optional (address "127.0.0.1") (port 8675))
  "Fire up a server thread that will listen for connections."
  (format t "Starting server...~%")
  (let ((socket (usocket:socket-listen
                  address port
                  :reuse-address t
                  ;; have to specify element-type here too because usocket+CCL
                  ;; fucks it up if you only specify it in socket-accept
                  :element-type '(unsigned-byte 8))))
    (setf *server-thread*
          (run-in-thread (format nil "NREPL Server (~a/~a)" address port)
            (unwind-protect
                (accept-connections socket)
              (format t "Closing server socket...~%")
              (usocket:socket-close socket))))))

(defun stop-server ()
  "Kill the server thread, if it exists."
  (let ((s (shiftf *server-thread* nil)))
    (when s
      (bt:destroy-thread s))))

