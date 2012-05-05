(in-package #:cl-user)

(defpackage #:explicit-bind
  (:use #:cl)
  (:export #:bind
           #:with-shadowed-bindings
           #:flet*))

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

(defpackage #:explicit-bind-merger
  (:nicknames #:eb-merger)
  (:use #:cl)
  (:shadow #:find
           #:function)
  (:export #:define
           #:find
           #:merger
           #:name
           #:function
           #:lambda-list))
