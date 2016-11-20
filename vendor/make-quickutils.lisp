(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :hash-table-keys
               :rcurry
               :subdivide
               :with-gensyms

               )
  :package "NREPL.QUICKUTILS")
