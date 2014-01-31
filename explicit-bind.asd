(asdf:defsystem #:explicit-bind

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Effectively enables intermixing of any number of LET, LET*, (an enhanced version of) FLET, MULTIPLE-VALUE-BIND and DESTRUCTURING-BIND constructs, at the cost of only one level of indentation."

  :depends-on (#:bubble-operator-upwards
               #:with-shadowed-bindings)

  :version "0.1"
  :serial cl:t
  :components ((:file "package")

	       (:file "flet-star")

               (:file "expand")

	       (:file "merge")
	       (:file "main")))
