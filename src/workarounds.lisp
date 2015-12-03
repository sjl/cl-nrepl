(in-package #:nrepl)

; WELCOME TO THE JUNGLE
; WE'VE GOT HACKS AND STRINGS
; YOU CAN GET ANYTHING YOU WANT
; BUT IT BETTER BE HACKS OR STRINGS

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

(defmacro defworkaround (name op message-binding &rest body)
  (let ((fallback (gensym)))
    `(defun ,name (,fallback)
       (lambda (,message-binding)
         (handle-workaround
           ,message-binding ,fallback ,op
           ,@body)))))


(defworkaround workaround-fireplace-classpath "eval" message
  (workaround-matches
    '("(do (println \"success\") "
      "(symbol (str (System/getProperty \"path.separator\") "
      "(System/getProperty \"java.class.path\"))))"))
  (respond message (make-map "value" ":")))

(defworkaround workaround-fireplace-pathsep "eval" message
  (workaround-matches
    '("[(System/getProperty \"path.separator\") "
      "(System/getProperty \"java.class.path\")]"))
  (respond message (make-map "value" "[\"/\" \":\"]")))

(defworkaround workaround-fireplace-star "eval" message
  ((lambda (code)
     (member code '("(*1 1)" "(*2 2)" "(*3 3)") :test #'equal)))
  (respond message (make-map "value" "Not yet implemented, sorry :(")))

(defworkaround workaround-fireplace-fakepathsep "eval" message
  ; lol what in the fuck even is this for?
  (workaround-matches
    '("[(System/getProperty \"path.separator\") "
      "(System/getProperty \"fake.class.path\")]"))
  (respond message (make-map "value" "[\"/\" \"None\"]")))

(defworkaround workaround-fireplace-macroexpand-all "eval" message
  ; this should really do a macroexpand-all but this'll have to do for now
  (starts-with "(clojure.walk/macroexpand-all (quote")
  ; TODO: Fix the extra done status message here
  (funcall h (with-map message "code"
                       (lambda (s)
                         (ppcre:regex-replace
                           "clojure.walk/macroexpand-all"
                           s
                           "macroexpand")))))

; TODO: (def-workaround (+ "[(symbol (str \"\\n\\b\" (apply str (interleave "
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

