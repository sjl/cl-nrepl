(in-package :nrepl)

(defvar *sessions* (make-hash-table :test #'equal))
(defvar *session* nil)


(defun make-session ()
  (fset:empty-map))

(defun clear-sessions! ()
  (clrhash *sessions*))

(defun register-session! (id session)
  (setf (gethash id *sessions*) session))

(defun remove-session! (id)
  (remhash id *sessions*))

(defun get-session (id)
  (gethash id *sessions*))

(defun get-sessions ()
  (hash-table-keys *sessions*))


(defun wrap-session (handler)
  "Handle wrapping incoming messages in sessions.

  If a message contains a session key, look up that session in the list of
  registered sessions and bind it into *session*.

  If a message comes in without a session id, create a new session for it and
  patch the session id into the message before continuing on down the
  middleware stack.  Also binds the session into *session*.

  Note that this implicit creation will NOT register the session into the main
  map of sessions.  To register the session so it'll stick around the client
  needs to do a `clone` op.

  "
  (lambda (message)
    (let* ((session-id (fset:lookup message "session"))
           (session (if session-id
                      (get-session session-id)
                      (make-session)))
           (session-id (or session-id (random-uuid)))
           (*session* session))
      (funcall handler (fset:with message "session" session-id)))))

(define-middleware wrap-session-ls "ls-sessions" message
  (respond message
           (make-map "status" '("done")
                     "sessions" (get-sessions))))

(define-middleware wrap-session-close "close" message
  (remove-session! (fset:lookup message "session"))
  (respond message (make-map "status" '("session-closed"))))

(define-middleware wrap-session-clone "clone" message
  (let ((new-id (register-session! (random-uuid)
                                   (fset:lookup message "session"))))
    (respond message (make-map "status" '("done") "new-session" new-id))))

