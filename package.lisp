(in-package #:cl-user)

(defpackage #:explicit-bind
  (:use #:cl)
  (:export #:bind
           #:with-shadowed-bindings
           #:flet*))

;; Don't :use. Use package-qualified symbols.
(defpackage #:explicit-bind-expander
  (:nicknames #:eb-expander)
  (:use #:cl)
  (:shadow #:find
           #:function)
  (:export #:define
           #:expand
           #:find
           #:expander
           #:name
           #:function
           #:specs-lambda-list
           #:forms-lambda-list))

;; Don't :use. Use package-qualified symbols.
(defpackage #:explicit-bind-merger
  (:nicknames #:eb-merger)
  (:use #:cl)
  (:shadow #:find
           #:function
           #:merge)
  (:export #:define
           #:merge1
           #:merge
           #:find
           #:merger
           #:name
           #:function
           #:lambda-list))
