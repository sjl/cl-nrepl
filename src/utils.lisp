(in-package :nrepl)

(defvar *log* *error-output*)


(defun make-map (&rest keyvals)
  "Create an fset map of the given riffle of keys and values."
  (fset:convert 'fset:map (subdivide keyvals 2)))

(defun with-when (map &rest keyvals)
  "Add the items in the `keyvals` riffle with non-nil values to `map`."
  (labels ((build (map keyvals)
             (if (null keyvals)
               map
               (destructuring-bind (k v &rest remaining) keyvals
                 (build (if v
                          (fset:with map k v)
                          map)
                        remaining)))))
    (build map keyvals)))

(defun read-all-from-string (s)
  "Read all forms in `s` and return them as a list."
  (labels ((read-next-from-string (s results)
             (if (equal (string-trim " " s) "")
               results
               (multiple-value-bind (i pos) (read-from-string s)
                 (read-next-from-string (subseq s pos) (cons i results))))))
    (nreverse (read-next-from-string s ()))))

(defun random-uuid ()
  "Return a random UUID as a string."
  (format nil "~a" (uuid:make-v4-uuid)))

(defun log-message (&rest args)
  (apply #'format *log* args)
  (force-output *log*))

(defun respond (message response)
  "Respond to `message` with the `response` map.

  Takes care of finding the transport and patching the message and session IDs
  into the response.

  "
  (funcall (fset:lookup message "transport")
           (with-when response
             "id" (fset:lookup message "id")
             "session" (fset:lookup message "session"))))


(defun parse-in-package (in-package)
  (if (or (null in-package) (string= in-package ""))
    *package*
    (or (find-package (read-from-string in-package)) *package*)))


(defmacro when-found ((var lookup-expr) &body body)
  "Perform `body` with `var` bound to the results of `lookup-expr`, when valid.

  `lookup-expr` should be an expression that returns two values, the first being
  the result (which will be bound to `var`) and the second indicating whether
  the lookup was successful.  The standard `gethash` is an example of a function
  that behaves like this.

  Instead of:
  (multiple-value-bind (val found) (gethash :foo hash)
    (when found
      body))

  (when-found (val (gethash :foo hash))
    body)

  "
  (with-gensyms (found)
    `(multiple-value-bind (,var ,found) ,lookup-expr
      (when ,found
        ,@body))))
