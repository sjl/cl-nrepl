(in-package #:nrepl)

;;;; In/out
(defun write-object (socket-stream lock map)
  "Bencode and write `map` to `socket-stream` while holding `lock`."
  (bt:with-lock-held (lock)
    (bencode:encode map socket-stream)
    (force-output socket-stream)))

(defun read-object (socket-stream)
  "Read and bdecode a map from `socket-stream`."
  (fset:convert 'fset:map (bencode:decode socket-stream)))


;;;; FSet+Bencode
(defmethod bencode:encode ((fm fset:map) stream &key &allow-other-keys)
  (bencode:encode (fset:convert 'hash-table fm) stream))

(defmethod bencode:encode ((fs fset:set) stream &key &allow-other-keys)
  (bencode:encode (fset:convert 'list fs) stream))

(defmethod bencode:encode ((fb fset:bag) stream &key &allow-other-keys)
  (bencode:encode (fset:convert 'list fb) stream))

(defmethod bencode:encode ((fb fset:seq) stream &key &allow-other-keys)
  (bencode:encode (fset:convert 'list fb) stream))

