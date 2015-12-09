(in-package #:nrepl)


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
    ; just kill me please
    #'workaround-fireplace-classpath
    #'workaround-fireplace-pathsep
    #'workaround-fireplace-star
    #'workaround-fireplace-fakepathsep
    #'workaround-fireplace-macroexpand-all
    #'wrap-describe
    #'wrap-load-file
    #'wrap-macroexpand
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
  (l "Handling message: vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv~%~A~%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~%" message)
  (funcall (build-handler #'handle-base (middleware)) message))

(defun handle-message (socket lock)
  "Read a single message from the socket and handle it."
  (let ((message (fset:with (read-object socket)
                            "transport" (curry #'write-object socket lock))))
    (handle message)))

(defun handler (socket lock)
  "Read a series of messages from the socket, handling each."
  (p "Client connected...")
  (handler-case (loop (handle-message socket lock))
    (end-of-file () nil))
  (p "Client disconnected..."))


;;;; Server
(defvar *server-thread* nil)

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

(defun start-server (&optional (address "localhost") (port 8675))
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

