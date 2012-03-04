(in-package #:explicit-bind)

(defun %function-name-core (name)
  (etypecase name
    ((cons (eql setf)
	   (cons symbol null))
     (second name))
    (symbol name)))

(defun %gensymed-function-name (name)
  (gensym (string (%function-name-core name))))

;; PARSE-BODY from Alexandria.
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

(defun extract-function-declarations (declarations &key name)
  (let ((core (and name
		   (%function-name-core name)))
	target-declarations
	other-declarations)
    (labels ((correct-name-p (name)
	       (or (not core)
		   (eq name core)))
	     (split (function list key)
	       (let (a b)
		 (dolist (element list
			  (values (nreverse a)
				  (nreverse b)))
		   (if (funcall function (funcall key element))
		       (push element a)
		       (push element b)))))
	     (bof (wrapper function list &key (key #'identity))
	       (multiple-value-bind (targets others)
		   (split function list key)
		 (when targets
		   (push (funcall wrapper targets) target-declarations))
		 (when others
		   (push (funcall wrapper others) other-declarations)))))
      (dolist (declaration declarations
	       (values (nreverse target-declarations)
		       (nreverse other-declarations)))
	(case (first declaration)
	  (ftype
	   (bof (lambda (names)
		  `(ftype ,(second declaration) ,@names))
		#'correct-name-p
		(cddr declaration)
		:key #'%function-name-core))
	  ((inline notinline)
	   (bof (lambda (names)
		  `(,(first declaration) ,@names))
		#'correct-name-p
		(cdr declaration)
		:key #'%function-name-core))
	  ((ignore ignorable dynamic-extent)
	   (bof (lambda (names)
		  `(,(first declaration) ,@names))
		(lambda (name)
		  (and (typep name '(cons (eql function) (cons t null)))
		       (correct-name-p
			(%function-name-core (second name)))))
		(cdr declaration)))
	  (t (push declaration other-declarations)))))))

(defun flet*-undefined (name)
  (lambda (&rest args)
    (error "Tried to call undefined ~S function ~S with args ~S"
	   'flet* name args)))

(defmacro flet* (definitions &body body)
  (multiple-value-bind (body declarations) (parse-body body)
    (setf declarations
	  (mapcan (lambda (declaration)
		    (copy-seq (cdr declaration)))
		  declarations))
    (let ((defcount (length definitions)))
      (if definitions
	  (flet ((extract-declarations (function-name)
		   (multiple-value-bind (targets others)
		       (extract-function-declarations
			declarations :name function-name)
		     (setf declarations others)
		     targets))
		 (decls (declarations)
		   (when declarations
		     `((declare ,@declarations)))))
	    (first
	     (reduce
	      (lambda (definition body)
		(destructuring-bind (name form) definition
		  (let* ((extracted-declarations
			  (extract-declarations name))
			 (remaining-declarations
			  (when (zerop (decf defcount))
			    declarations)))
		    (list
		     (if (typep form '(cons (eql lambda)))
			 `(flet ((,name ,@(cdr form)))
			    ,@(decls (append
				      remaining-declarations
				      extracted-declarations))
			    ,@body)
			 (let ((function-var
				(%gensymed-function-name name)))
			   `(let ((,function-var
				   (or ,form
				       (flet*-undefined ',name))))
			      ,@(decls
				 (append
				  `((type (or function
					      symbol
					      null
					      (cons (eql setf)
						    (cons symbol null)))
					  ,function-var))
				  remaining-declarations))
			      (flet ((,name (&rest args)
				       (apply ,function-var args)))
				,@(decls extracted-declarations)
				,@body))))))))
	      definitions
	      :from-end t
	      :initial-value body)))
	  `(locally ,@declarations
	     ,@body)))))
