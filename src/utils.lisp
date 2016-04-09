(in-package #:nrepl)

(defvar *log* *error-output*)


(defun pairs (sequence)
  "Return a list of cons pairs of the items of the riffle `sequence`."
  (loop :for (a b) :on sequence :by #'cddr
        :collect (cons a b)))

(defun make-map (&rest keyvals)
  "Create an fset map of the given riffle of keys and values."
  (fset:convert 'fset:map (pairs keyvals)))

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

(defun curry (fn &rest curried-args)
  (lambda (&rest args)
    (apply fn (append curried-args args))))

(defun random-uuid ()
  "Return a random UUID as a string."
  (format nil "~a" (uuid:make-v4-uuid)))

(defun hash-keys (h)
  (loop :for key :being :the :hash-keys :of h
        :collect key))


(defun l (&rest args)
  (apply #'format *log* args))

(defun p (o)
  (format *log* "~a~%" o)
  o)


(defun respond (message response)
  "Respond to `message` with the `response` map.

  Takes care of finding the transport and patching the message and session IDs
  into the response.

  "
  (funcall (fset:lookup message "transport")
           (with-when response
             "id" (fset:lookup message "id")
             "session" (fset:lookup message "session"))))


(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~%~{~{    (~s : ~s)~}~%~}}"
          (loop :for key :being :the :hash-keys :of object
                :using (hash-value value)
                :collect (list key value))))

