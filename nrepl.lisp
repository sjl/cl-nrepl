; (in-package :nrepl)

(ql:quickload "bencode")
(ql:quickload "usocket")
(ql:quickload "flexi-streams")
(ql:quickload "bordeaux-threads")
(ql:quickload "uuid")
(ql:quickload "fset")
(ql:quickload "cl-ppcre")

(require 'sb-introspect)


;;;; Variables ----------------------------------------------------------------
(defvar *server-thread* nil)
(defvar *log* *error-output*)


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

(defmacro if-let (bindings then else)
  `(let (,@bindings)
     (if (and ,@(mapcar #'car bindings))
       ,then
       ,else)))

(defun pairs (l)
  (loop for (a b) on l by #'cddr
        collect (cons a b)))

(defun make-map (&rest keyvals)
  (fset:convert 'fset:map (pairs keyvals)))

(defun set-when (h &rest keyvals)
  (loop for (key val) on keyvals by #'cddr
        do (when val (setf (gethash key h) val))))

(defun with-when (m &rest keyvals)
  (labels ((build (m keyvals)
             (if (not keyvals)
               m
               (destructuring-bind (k v &rest remaining) keyvals
                 (build (if v
                          (fset:with m k v)
                          m)
                        remaining)))))
    (build m keyvals)))

(defun with-map (m key f)
  (let ((val (fset:lookup m key)))
    (fset:with m key (funcall f val))))

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

(defun starts-with (prefix str)
  (string= str prefix :end1 (min (length str)
                                 (length prefix))))

(defun l (&rest args)
  (apply #'format *log* args))

(defun p (o)
  (format *log* "~a~%" o)
  o)


;;;; Sockets ------------------------------------------------------------------
(defun get-stream (sock)
  "Make a flexi stream of the kind bencode wants from the socket."
  (flex:make-flexi-stream
    (usocket:socket-stream sock)
    :external-format :utf-8))


(defun write-object (socket lock o)
  "Bencode and write a map M to SOCKET while holding LOCK."
  (bt:with-lock-held (lock)
                     (bencode:encode o (get-stream socket))
                     (force-output (get-stream socket))))

(defun read-object (socket)
  "Read a map (and bdecode it) from *socket*."
  (fset:convert 'fset:map
                ; fireplace's bencoding is fucked.
                ; just ignore it its fine
                (handler-bind ((error #'continue))
                  (bencode:decode (get-stream socket)))))


;;; Patch in support for writing fset data types to bencode
(defmethod bencode:encode ((fm fset:map) stream &key &allow-other-keys)
  (bencode:encode (fset:convert 'hash-table fm) stream))

(defmethod bencode:encode ((fs fset:set) stream &key &allow-other-keys)
  (bencode:encode (fset:convert 'list fs) stream))

(defmethod bencode:encode ((fb fset:bag) stream &key &allow-other-keys)
  (bencode:encode (fset:convert 'list fb) stream))

(defmethod bencode:encode ((fb fset:seq) stream &key &allow-other-keys)
  (bencode:encode (fset:convert 'list fb) stream))


;;;; Workarounds --------------------------------------------------------------
;;; welcome to the jungle
;;; we've got hacks and strings
;;; you can get anything you want
;;; but it better be hacks or strings

; (import traceback)

(defun workaround-matches (l code)
  (equal (apply #'concatenate 'string l)
         code))

(defmacro handle-workaround (message fallback op check &rest body)
  `(if (and (equal ,op (fset:lookup ,message "op"))
            (,@check (fset:lookup ,message "code")))
     (progn
       ,@body
       (respond ,message (make-map "status" '("done"))))
     (funcall ,fallback ,message)))

(defun workaround-fireplace-classpath (h)
  (lambda (message)
    (handle-workaround
      message h "eval"
      (workaround-matches
        '("(do (println \"success\") "
          "(symbol (str (System/getProperty \"path.separator\") "
          "(System/getProperty \"java.class.path\"))))"))
      (respond message (make-map "value" ":")))))

(defun workaround-fireplace-pathsep (h)
  (lambda (message)
    (handle-workaround
      message h "eval"
      (workaround-matches
        '("[(System/getProperty \"path.separator\") "
          "(System/getProperty \"java.class.path\")]"))
      (respond message (make-map "value" "[\"/\" \":\"]")))))

(defun workaround-fireplace-star (h)
  (lambda (message)
    (handle-workaround
      message h "eval"
      ((lambda (code)
         (member code '("(*1 1)" "(*2 2)" "(*3 3)") :test #'equal)))
      (respond message (make-map "value" "Not yet implemented, sorry :(")))))

(defun workaround-fireplace-fakepathsep (h)
  ; lol what in the fuck even is this for?
  (lambda (message)
    (handle-workaround
      message h "eval"
      (workaround-matches
        '("[(System/getProperty \"path.separator\") "
          "(System/getProperty \"fake.class.path\")]"))
      (respond message (make-map "value" "[\"/\" \"None\"]")))))

(defun workaround-fireplace-macroexpand-all (h)
  ; this should really do a macroexpand-all but this'll have to do for now
  (lambda (message)
    (handle-workaround
      message h "eval"
      (starts-with "(clojure.walk/macroexpand-all (quote")
      ; TODO: Fix the extra done status message here
      (funcall h (with-map message "code"
                           (lambda (s)
                             (ppcre:regex-replace
                               "clojure.walk/macroexpand-all"
                               s
                               "macroexpand")))))))

; (def-workaround (+ "[(symbol (str \"\\n\\b\" (apply str (interleave "
;                    "(repeat \"\\n\") (map str (.getStackTrace *e)))) "
;                    "\"\\n\\b\\n\")) *3 *2 *1]")
;                 [session msg]
;                 (let [items []]
;                   (with [session.lock]
;                     (for [i (traceback.extract-tb session.last-traceback)]
;                       (.append items (.format "{}({}:{})"
;                                               (get i 2)
;                                               (first i)
;                                               (second i)))))
;                   (+ "(quote " "[\n\b\n" (.join "\n" items) "\n\b\n nil nil nil]" ")")))


;;;; NREPL --------------------------------------------------------------------
;;; Utils
(defun respond (message response)
  (funcall (fset:lookup message "transport")
           (with-when response
                      "id" (fset:lookup message "id")
                      "session" (fset:lookup message "session"))))

(defmacro handle-op (message op fallback &rest body)
  `(if (equal ,op (fset:lookup ,message "op"))
     (progn ,@body)
     (funcall ,fallback ,message)))


;;; Sessions
(defvar *sessions* (make-hash-table :test #'equal))
(defvar *session* nil)

(defun clear-sessions! ()
  (setf *sessions* (make-hash-table :test #'equal)))

(defun create-session ()
  (fset:empty-map))

(defun register-session! (id session)
  (setf (gethash id *sessions*) session))

(defun remove-session! (id)
  (remhash id *sessions*))

(defun get-session (id)
  (gethash id *sessions*))

(defun get-sessions ()
  (hash-keys *sessions*))


(defun wrap-session (h)
  "Handle wrapping incoming messages in sessions.

   If a message contains a session key, look up that session in the list of
   registered sessions and bind it into *session*.

   If a message comes in without a session id, create a new session for it and
   patch the session id into the message before continuing on down the
   middleware stack.  Also binds the session into *session*.  Note that this
   will NOT register the session into the main map of sessions.

   "
  (lambda (message)
    (let* ((session-id (fset:lookup message "session"))
           (session (if session-id
                      (get-session session-id)
                      (create-session)))
           (session-id (or session-id (random-uuid)))
           (*session* session))
      (funcall h (fset:with message "session" session-id)))))

(defun wrap-session-ls (h)
  (lambda (message)
    (handle-op
      message "ls-sessions" h
      (respond message
               (make-map "status" '("done")
                         "sessions" (get-sessions))))))

(defun wrap-session-close (h)
  (lambda (message)
    (handle-op
      message "close" h
      (remove-session! (fset:lookup message "session"))
      (respond message (make-map "status" '("session-closed"))))))

(defun wrap-session-clone (h)
  (lambda (message)
    (handle-op
      message "clone" h
      (let ((new-id (register-session! (random-uuid)
                                       (fset:lookup message "session"))))
        (respond message (make-map "status" '("done") "new-session" new-id))))))


;;; Eval
(defclass evaluator ()
  ((standard-input :initarg :in :reader in)
   (standard-output :initarg :out :reader out)
   (standard-error :initarg :err :reader err)))

(defun handle-base (message)
  (respond message (make-map "status" '("unknown-op"))))


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
      (let* ((s (find-symbol-harder (fset:lookup message "symbol"))))
        (respond message
                 (with-when
                   (make-map "status" '("done"))
                   "type-docstring" (documentation s 'type)
                   "structure-docstring" (documentation s 'structure)
                   "variable-docstring" (documentation s 'variable)
                   "setf-docstring" (documentation s 'setf)
                   "function-docstring" (documentation s 'function)
                   "function-arglist" (when-let ((arglist (find-lambda-list s)))
                                        (prin1-to-string arglist))))))))


;;; Describe
(defun make-version-map (major minor incremental)
  (make-map "major" major
            "minor" minor
            "incremental" incremental
            "version-string" (format nil "~d.~d.~d" major minor incremental)))

(defun wrap-describe (h)
  (lambda (message)
    (handle-op
      message "describe" h
      (respond message
               (make-map "status" '("done")
                         "versions" (make-map
                                      "lisp" (make-version-map 0 0 0)
                                      "cl-nrepl" (make-version-map 0 0 0)
                                      ; we're not nrepl but fireplace wants this
                                      "nrepl" (make-version-map 0 2 0))
                         "ops" (make-map))))))

; {'aux': {'current-ns': 'user'},
;  'ops': {'clone': {'doc': 'Clones the current session, returning the ID of the newly-created session.',
;                    'optional': {'session': 'The ID of the session to be cloned; if not provided, a new session with default bindings is created, and mapped to the returned session ID.'},
;                    'requires': {},
;                    'returns': {'new-session': 'The ID of the new session.'}},
;          'close': {'doc': 'Closes the specified session.',
;                    'optional': {},
;                    'requires': {'session': 'The ID of the session to be closed.'},
;                    'returns': {}},
;          'describe': {'doc': 'Produce a machine- and human-readable directory and documentation for the operations supported by an nREPL endpoint.',
;                       'optional': {'verbose?': 'Include informational detail for each "op"eration in the return message.'},
;                       'requires': {},
;                       'returns': {'aux': 'Map of auxilliary data contributed by all of the active nREPL middleware via :describe-fn functions in their descriptors.',
;                                   'ops': 'Map of "op"erations supported by this nREPL endpoint',
;                                   'versions': 'Map containing version maps (like *clojure-version*, e.g. major, minor, incremental, and qualifier keys) for values, component names as keys. Common keys include "nrepl" and "clojure".'}},
;          'eval': {'doc': 'Evaluates code.',
;                   'optional': {'eval': 'A fully-qualified symbol naming a var whose function value will be used to evaluate [code], instead of `clojure.core/eval` (the default).',
;                                'id': 'An opaque message ID that will be included in responses related to the evaluation, and which may be used to restrict the scope of a later "interrupt" operation.'},
;                   'requires': {'code': 'The code to be evaluated.',
;                                'session': 'The ID of the session within which to evaluate the code.'},
;                   'returns': {'ex': 'The type of exception thrown, if any. If present, then `values` will be absent.',
;                               'ns': '*ns*, after successful evaluation of `code`.',
;                               'root-ex': 'The type of the root exception thrown, if any. If present, then `values` will be absent.',
;                               'values': 'The result of evaluating `code`, often `read`able. This printing is provided by the `pr-values` middleware, and could theoretically be customized. Superseded by `ex` and `root-ex` if an exception occurs during evaluation.'}},
;          'interrupt': {'doc': 'Attempts to interrupt some code evaluation.',
;                        'optional': {'interrupt-id': 'The opaque message ID sent with the original "eval" request.'},
;                        'requires': {'session': 'The ID of the session used to start the evaluation to be interrupted.'},
;                        'returns': {'status': '\'interrupted\' if an evaluation was identified and interruption will be attempted\n\'session-idle\' if the session is not currently evaluating any code\n\'interrupt-id-mismatch\' if the session is currently evaluating code sent using a different ID than specified by the "interrupt-id" value '}},
;          'load-file': {'doc': 'Loads a body of code, using supplied path and filename info to set source file and line number metadata. Delegates to underlying "eval" middleware/handler.',
;                        'optional': {'file-name': 'Name of source file, e.g. io.clj',
;                                     'file-path': 'Source-path-relative path of the source file, e.g. clojure/java/io.clj'},
;                        'requires': {'file': 'Full contents of a file of code.'},
;                        'returns': {'ex': 'The type of exception thrown, if any. If present, then `values` will be absent.',
;                                    'ns': '*ns*, after successful evaluation of `code`.',
;                                    'root-ex': 'The type of the root exception thrown, if any. If present, then `values` will be absent.',
;                                    'values': 'The result of evaluating `code`, often `read`able. This printing is provided by the `pr-values` middleware, and could theoretically be customized. Superseded by `ex` and `root-ex` if an exception occurs during evaluation.'}},
;          'ls-sessions': {'doc': 'Lists the IDs of all active sessions.',
;                          'optional': {},
;                          'requires': {},
;                          'returns': {'sessions': 'A list of all available session IDs.'}},
;          'stdin': {'doc': 'Add content from the value of "stdin" to *in* in the current session.',
;                    'optional': {},
;                    'requires': {'stdin': 'Content to add to *in*.'},
;                    'returns': {'status': 'A status of "need-input" will be sent if a session\'s *in* requires content in order to satisfy an attempted read operation.'}}},


;;; Plumbing
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
  (handler-case (loop (handle-message socket lock))
    (end-of-file () nil)))


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

