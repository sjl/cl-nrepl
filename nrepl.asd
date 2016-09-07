(asdf:defsystem #:nrepl
  :name "nrepl"
  :description "An implementation of the NREPL protocol for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "EPL"
  :version "0.0.1"

  :depends-on (#:bencode
               #:usocket
               #:flexi-streams
               #:bordeaux-threads
               #:uuid
               #:fset
               #:cl-ppcre
               #:split-sequence
               #:dissect
               #:trivial-arguments)

  :serial t
  :components
  ((:file "package")
   (:module "src"
    :components ((:file "utils")
                 (:file "sockets")
                 (:file "evaluation")
                 (:module "middleware"
                  :components ((:file "core")
                               (:file "describe")
                               (:file "documentation")
                               (:file "macroexpand")
                               (:file "eval")
                               (:file "load-file")
                               (:file "session")))
                 (:file "server")))))

