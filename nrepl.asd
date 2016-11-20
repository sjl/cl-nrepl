(asdf:defsystem :nrepl
  :name "nrepl"
  :description "An implementation of the NREPL protocol for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "EPL"
  :version "0.0.1"

  :depends-on (
               :bencode
               :bordeaux-threads
               :dissect
               :flexi-streams
               :fset
               :split-sequence
               :trivial-arguments
               :usocket
               :uuid
               )

  :serial t
  :components
  ((:file "package")
   (:module "src" :serial t
    :components ((:file "utils")
                 (:file "sockets")
                 (:file "evaluation")
                 (:module "middleware" :serial t
                  :components ((:file "core")
                               (:file "describe")
                               (:file "documentation")
                               (:file "macroexpand")
                               (:file "eval")
                               (:file "load-file")
                               (:file "session")))
                 (:file "server")))))

