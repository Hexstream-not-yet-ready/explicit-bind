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
	(keys (gensym (string '#:keys)))
        (op-gensym (unless op (gensym (string '#:op)))))
    `(setf (find ',name)
	   (make-instance
	    'merger
	    :name ',name
	    :function
	    (lambda (,spec ,body-var ,keys)
	      (destructuring-bind ((,(or op op-gensym) ,@args)
                                   ,body-var
                                   ,keys-pattern)
		  (list ,spec ,body-var ,keys)
                ,@(when op-gensym `((declare (ignorable ,op-gensym))))
		,@body))
	    :lambda-list ',args))))

(defun merge1 (spec body keys)
  (funcall (function (find (first spec))) spec body keys))

(defun merge (specs body &key keys)
  (let ((body (reduce (lambda (specs body)
                        (multiple-value-bind (body new-keys)
                            (merge1 specs body keys)
                          (setf keys new-keys)
                          body))
                      specs
                      :from-end t
                      :initial-value body))
        (declarations (getf keys :declarations)))
    (declare (ignore declarations))
    body))


(define declare ((nil &rest declaration-specifiers) body keys)
  (values body
          (if declaration-specifiers
              (list* :declarations (append declaration-specifiers
                                           (getf keys :declarations))
                     keys)
              keys)))

(define shadow ((nil &rest binding-names) body keys)
  (values (list `(explicit-bind:with-shadowed-bindings ,binding-names
                   ,@body))
          keys))

(define progn ((nil &body forms) body keys)
  (values (append forms body) keys))

(define variable ((nil name form) body keys)
  (values (list `(let ((,name ,form))
                   ,@body))
          keys))

(define cl:function ((nil name form) body keys)
  (values (list `(explicit-bind:flet* ((,name ,form))
                   ,@body))
          keys))

(define :destructuring ((nil lambda-list form) body keys)
  (values (list `(destructuring-bind ,lambda-list ,form
                   ,@body))
          keys))

(define :accessors ((nil accessor-specifications instance-form) body keys)
  (values (list `(with-accessors ,accessor-specifications ,instance-form
                   ,@body))
          keys))

(define :slots ((nil slot-specifications instance-form) body keys)
  (values (list `(with-slots ,slot-specifications ,instance-form
                   ,@body))
          keys))
