(in-package #:explicit-bind-merger)

(defvar *mergers* (make-hash-table :test 'eq))

(defclass merger ()
  ((name :initarg :name
	 :reader name
	 :type (and symbol (not null)))
   (function :initarg :function
	     :reader function)
   (lambda-list :initarg :lambda-list
		:reader lambda-list
		:type list)))

(defun find (name &key (errorp t))
  (check-type name symbol)
  (or (gethash name *mergers*)
      (when errorp
	(error "There is no ~S called ~S." 'merger name))))

(defun (setf find) (new name &key (errorp t))
  (declare (ignore errorp))
  (check-type new merger)
  (check-type name (and symbol (not null)))
  (setf (gethash name *mergers*) new))

(defmacro define (name ((op &rest args) body-var keys-pattern) &body body)
  (check-type name (and symbol (not null)))
  (let ((spec (gensym (string '#:spec)))
	(keys (gensym (string '#:keys))))
    `(setf (find ',op)
	   (make-instance
	    'merger
	    :name ',name
	    :function
	    (lambda (,spec ,body-var ,keys)
	      (destructuring-bind ((,op ,@args) ,body-var ,keys-pattern)
		  (list ,spec ,body-var ,keys)
		,@body))
	    :lambda-list ',args))))

(define declare ((op &rest declaration-specifiers) body keys)
  (declare (ignore op keys declaration-specifiers))
  (values body))

(define shadow ((op &rest binding-names) body keys)
  (declare (ignore op keys))
  (list `(with-shadowed-bindings ,binding-names
	   ,@body)))

(define progn ((op &body forms) body keys)
  (declare (ignore op))
  (values (append forms body) keys))

(define variable ((op name form) body keys)
  (declare (ignore op keys))
  (list `(let ((,name ,form))
	   ,@body)))

(define cl:function ((op name form) body keys)
  (declare (ignore op keys))
  (list `(flet* ((,name ,form))
	   ,@body)))

(define :destructuring ((op lambda-list form) body keys)
  (declare (ignore op keys))
  (list `(destructuring-bind ,lambda-list ,form
	   ,@body)))

(define :accessors ((op accessor-specifications instance-form) body keys)
  (declare (ignore op keys))
  (list `(with-accessors ,accessor-specifications ,instance-form
	   ,@body)))

(define :slots ((op slot-specifications instance-form) body keys)
  (declare (ignore op keys))
  (list `(with-slots ,slot-specifications ,instance-form
	   ,@body)))
