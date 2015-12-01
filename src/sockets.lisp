(in-package #:nrepl)

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

