(in-package #:explicit-bind)

(defvar *explicit-bind-expanders* (make-hash-table :test 'eq))

(defclass explicit-bind-expander ()
  ((name :initarg :name
	 :reader explicit-bind-expander-name
	 :type (and symbol (not null)))
   (function :initarg :function
	     :reader explicit-bind-expander-function
	     :type (or function symbol))
   (specs-lambda-list :initarg :specs-lambda-list
		      :reader explicit-bind-expander-specs-lambda-list
		      :type list)
   (forms-lambda-list :initarg :forms-lambda-list
		      :reader explicit-bind-expander-forms-lambda-list
		      :type list)))

(defun find-explicit-bind-expander (name &key (errorp t))
  (check-type name symbol)
  (or (gethash name *explicit-bind-expanders*)
      (when errorp
	(error "There is no EXPLICIT-BIND expander called ~S." name))))

(defun (setf find-explicit-bind-expander) (new name &key (errorp t))
  (declare (ignore errorp))
  (check-type new explicit-bind-expander)
  (check-type name symbol)
  (setf (gethash name *explicit-bind-expanders*)
	new))

(defmacro define-explicit-bind-expander
    (name ((op &rest specifications) &rest forms) &body body)
  (etypecase name
    ((cons (eql or))
     `(progn ,@(mapcar
		(lambda (name)
		  `(define-explicit-bind-expander ,name
		       ((,op ,@specifications)
			,@forms)
		     ,@body))
		(cdr name))))
    (symbol
     (let ((binding (gensym (string '#:binding))))
       `(setf (find-explicit-bind-expander ',name)
	      (make-instance
	       'explicit-bind-expander
	       :name ',name
	       :function
	       (lambda (,binding)
		 (unless (typep ,binding '(cons (cons (eql ,name) t) t))
		   (error "Invalid binding spec ~S ~
                           passed to ~S explicit-bind expander."
			  ,binding ',name))
		 (destructuring-bind
		       ((,op ,@specifications) ,@forms) ,binding
		   ,@body))
	       :specs-lambda-list ',specifications
	       :forms-lambda-list ',forms))))))
