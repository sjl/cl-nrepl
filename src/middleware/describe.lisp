(in-package :nrepl)

(defun make-version-map (major minor incremental)
  (make-map "major" major
            "minor" minor
            "incremental" incremental
            "version-string" (format nil "~d.~d.~d" major minor incremental)))

(define-middleware wrap-describe "describe" message
  (respond message
           (make-map "status" '("done")
                     ;; TODO: find actual versions
                     "versions" (make-map
                                  "lisp" (make-version-map 0 0 0)
                                  "cl-nrepl" (make-version-map 0 0 0))
                     ;; TODO: fill in supported ops
                     "ops" (make-map))))


; {'aux': {'current-ns': 'user'},
;  'ops': {'clone': {'doc': 'Clones the current session, returning the ID of the newly-created session.',
;                    'optional': {'session': 'The ID of the session to be cloned; if not provided, a new session with default bindings is created, and mapped to the returned session ID.'},
;                    'requires': {},
;                    'returns': {'new-session': 'The ID of the new session.'}},
;          'close': {'doc': 'Closes the specified session.',
;                    'optional': {},
;                    'requires': {'session': 'The ID of the session to be closed.'},
;                    'returns': {}},
;          'describe': {'doc': 'Produce a machine- and human-readable directory and documentation for the operations supported by an nREPL endpoint.',
;                       'optional': {'verbose?': 'Include informational detail for each "op"eration in the return message.'},
;                       'requires': {},
;                       'returns': {'aux': 'Map of auxilliary data contributed by all of the active nREPL middleware via :describe-fn functions in their descriptors.',
;                                   'ops': 'Map of "op"erations supported by this nREPL endpoint',
;                                   'versions': 'Map containing version maps (like *clojure-version*, e.g. major, minor, incremental, and qualifier keys) for values, component names as keys. Common keys include "nrepl" and "clojure".'}},
;          'eval': {'doc': 'Evaluates code.',
;                   'optional': {'eval': 'A fully-qualified symbol naming a var whose function value will be used to evaluate [code], instead of `clojure.core/eval` (the default).',
;                                'id': 'An opaque message ID that will be included in responses related to the evaluation, and which may be used to restrict the scope of a later "interrupt" operation.'},
;                   'requires': {'code': 'The code to be evaluated.',
;                                'session': 'The ID of the session within which to evaluate the code.'},
;                   'returns': {'ex': 'The type of exception thrown, if any. If present, then `values` will be absent.',
;                               'ns': '*ns*, after successful evaluation of `code`.',
;                               'root-ex': 'The type of the root exception thrown, if any. If present, then `values` will be absent.',
;                               'values': 'The result of evaluating `code`, often `read`able. This printing is provided by the `pr-values` middleware, and could theoretically be customized. Superseded by `ex` and `root-ex` if an exception occurs during evaluation.'}},
;          'interrupt': {'doc': 'Attempts to interrupt some code evaluation.',
;                        'optional': {'interrupt-id': 'The opaque message ID sent with the original "eval" request.'},
;                        'requires': {'session': 'The ID of the session used to start the evaluation to be interrupted.'},
;                        'returns': {'status': '\'interrupted\' if an evaluation was identified and interruption will be attempted\n\'session-idle\' if the session is not currently evaluating any code\n\'interrupt-id-mismatch\' if the session is currently evaluating code sent using a different ID than specified by the "interrupt-id" value '}},
;          'load-file': {'doc': 'Loads a body of code, using supplied path and filename info to set source file and line number metadata. Delegates to underlying "eval" middleware/handler.',
;                        'optional': {'file-name': 'Name of source file, e.g. io.clj',
;                                     'file-path': 'Source-path-relative path of the source file, e.g. clojure/java/io.clj'},
;                        'requires': {'file': 'Full contents of a file of code.'},
;                        'returns': {'ex': 'The type of exception thrown, if any. If present, then `values` will be absent.',
;                                    'ns': '*ns*, after successful evaluation of `code`.',
;                                    'root-ex': 'The type of the root exception thrown, if any. If present, then `values` will be absent.',
;                                    'values': 'The result of evaluating `code`, often `read`able. This printing is provided by the `pr-values` middleware, and could theoretically be customized. Superseded by `ex` and `root-ex` if an exception occurs during evaluation.'}},
;          'ls-sessions': {'doc': 'Lists the IDs of all active sessions.',
;                          'optional': {},
;                          'requires': {},
;                          'returns': {'sessions': 'A list of all available session IDs.'}},
;          'stdin': {'doc': 'Add content from the value of "stdin" to *in* in the current session.',
;                    'optional': {},
;                    'requires': {'stdin': 'Content to add to *in*.'},
;                    'returns': {'status': 'A status of "need-input" will be sent if a session\'s *in* requires content in order to satisfy an attempted read operation.'}}},

