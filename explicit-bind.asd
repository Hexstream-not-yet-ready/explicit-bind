(asdf:defsystem #:explicit-bind

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "UNSUPPORTED. UNSTABLE? INCOMPLETE? Effectively enables intermixing of any number of LET, LET*, (an enhanced version of) FLET, MULTIPLE-VALUE-BIND and DESTRUCTURING-BIND constructs, at the cost of only one level of indentation."

  :version "0.1"
  :serial cl:t
  :components ((:file "package")
	       (:file "bubble-expand")
	       (:file "with-shadowed-bindings")
	       (:file "flet-star")
	       (:file "expand")
	       (:file "built-in")
	       (:file "merge")
	       (:file "main")))
