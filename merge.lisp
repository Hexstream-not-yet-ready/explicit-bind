(in-package #:explicit-bind)

(defvar *explicit-bind-mergers* (make-hash-table :test 'eq))

(defclass explicit-bind-merger ()
  ((name :initarg :name
	 :reader explicit-bind-merger-name
	 :type (and symbol (not null)))
   (function :initarg :function
	     :reader explicit-bind-merger-function)
   (lambda-list :initarg :lambda-list
		:reader explicit-bind-merger-lambda-list
		:type list)))

(defun find-explicit-bind-merger (name &key (errorp t))
  (check-type name symbol)
  (or (gethash name *explicit-bind-mergers*)
      (when errorp
	(error "There is no EXPLICIT-BIND merger called ~S." name))))

(defun (setf find-explicit-bind-merger) (new name &key (errorp t))
  (declare (ignore errorp))
  (check-type new explicit-bind-merger)
  (check-type name (and symbol (not null)))
  (setf (gethash name *explicit-bind-mergers*)
	new))

(defmacro define-explicit-bind-merger
    (name ((op &rest args) body-var keys-pattern) &body body)
  (check-type name (and symbol (not null)))
  (let ((spec (gensym (string '#:spec)))
	(keys (gensym (string '#:keys))))
    `(setf (find-explicit-bind-merger ',op)
	   (make-instance
	    'explicit-bind-merger
	    :name ',name
	    :function
	    (lambda (,spec ,body-var ,keys)
	      (destructuring-bind ((,op ,@args) ,body-var ,keys-pattern)
		  (list ,spec ,body-var ,keys)
		,@body))
	    :lambda-list ',args))))

(define-explicit-bind-merger declare ((op &rest declaration-specifiers) body keys)
  (declare (ignore op keys declaration-specifiers))
  (values body))

(define-explicit-bind-merger shadow ((op &rest binding-names) body keys)
  (declare (ignore op keys))
  (list `(with-shadowed-bindings ,binding-names
	   ,@body)))

(define-explicit-bind-merger progn
    ((op &body forms) body keys)
  (declare (ignore op))
  (values (append forms body) keys))

(define-explicit-bind-merger variable
    ((op name form) body keys)
  (declare (ignore op keys))
  (list `(let ((,name ,form))
	   ,@body)))

(define-explicit-bind-merger function
    ((op name form) body keys)
  (declare (ignore op keys))
  (list `(flet* ((,name ,form))
	   ,@body)))

(define-explicit-bind-merger :destructuring
    ((op lambda-list form) body keys)
  (declare (ignore op keys))
  (list `(destructuring-bind ,lambda-list ,form
	   ,@body)))

(define-explicit-bind-merger :accessors
    ((op accessor-specifications instance-form) body keys)
  (declare (ignore op keys))
  (list `(with-accessors ,accessor-specifications ,instance-form
	   ,@body)))

(define-explicit-bind-merger :slots
    ((op slot-specifications instance-form) body keys)
  (declare (ignore op keys))
  (list `(with-slots ,slot-specifications ,instance-form
	   ,@body)))
