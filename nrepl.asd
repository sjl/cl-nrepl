;;;; nrepl.asd

(asdf:defsystem #:nrepl
  :description "An implementation of the NREPL protocol for Common Lisp."
  :author "Steve Losh <steve@stevelosh.com>"
  :version "0.0.1"
  :license "EPL"
  :depends-on (#:bencode
               #:usocket
               #:flexi-streams
               #:bordeaux-threads
               #:uuid
               #:fset
               #:cl-ppcre
               #:split-sequence
               #+sbcl :sb-introspect)
  :components
  ((:file "package")
   (:module "src"
    :depends-on ("package")
    :components ((:file "utils" :depends-on ())
                 (:file "sockets" :depends-on ("utils"))
                 (:file "server" :depends-on ("utils"
                                              "sockets"
                                              "middleware"))
                 (:module "middleware"
                  :depends-on ("utils")
                  :serial t
                  :components
                  ((:file "core")
                   (:file "describe")
                   (:file "documentation")
                   (:file "load-file")
                   (:file "macroexpand")
                   (:file "eval")
                   (:file "session")))))))

