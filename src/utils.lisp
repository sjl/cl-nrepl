(in-package #:nrepl)

(defvar *log* *error-output*)


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


(defun respond (message response)
  (funcall (fset:lookup message "transport")
           (with-when response
                      "id" (fset:lookup message "id")
                      "session" (fset:lookup message "session"))))

(defmacro handle-op (message op fallback &rest body)
  `(if (equal ,op (fset:lookup ,message "op"))
     (progn ,@body)
     (funcall ,fallback ,message)))

(defmacro define-middleware (name op message-binding &rest body)
  (let ((fallback (gensym)))
  `(defun ,name (,fallback)
     (lambda (,message-binding)
       (handle-op ,message-binding ,op ,fallback
                  ,@body)))))
