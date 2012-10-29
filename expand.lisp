(in-package #:explicit-bind-expander)

(defvar *expanders* (make-hash-table :test 'eq))

(defclass expander ()
  ((%name :initarg :name
          :reader name
          :type (and symbol (not null)))
   (%function :initarg :function
              :reader function
              :type (or cl:function symbol))
   (%specs-lambda-list :initarg :specs-lambda-list
                       :reader specs-lambda-list
                       :type list)
   (%forms-lambda-list :initarg :forms-lambda-list
                       :reader forms-lambda-list
                       :type list)))

(defun find (name &key (errorp t))
  (check-type name symbol)
  (or (gethash name *expanders*)
      (when errorp
	(error "There is no ~S called ~S." 'expander name))))

(defun (setf find) (new name &key (errorp t))
  (declare (ignore errorp))
  (check-type new expander)
  (check-type name symbol)
  (setf (gethash name *expanders*) new))

(defmacro define (name ((op &rest specifications) &rest forms) &body body)
  (etypecase name
    ((cons (eql or))
     `(progn ,@(mapcar
		(lambda (name)
		  `(define ,name
		       ((,op ,@specifications)
			,@forms)
		     ,@body))
		(cdr name))))
    (symbol
     (let ((binding (gensym (string '#:binding))))
       `(setf (find ',name)
	      (make-instance
	       'expander
	       :name ',name
	       :function
	       (lambda (,binding)
		 (unless (typep ,binding '(cons (cons (eql ,name) t) t))
		   (error "Invalid binding spec ~S ~
                           passed to ~S ~S."
			  ,binding ',name 'expander))
		 (destructuring-bind
		       ((,op ,@specifications) ,@forms) ,binding
		   ,@body))
	       :specs-lambda-list ',specifications
	       :forms-lambda-list ',forms))))))

(defun expand (binding)
  (typecase binding
    ((cons (cons symbol t) t)
     (funcall (function (find (caar binding))) binding))
    ((cons symbol t)
     (destructuring-bind (var init-form) binding
       (funcall (function (find 'variable)) `((variable ,var) ,init-form))))
    (t (error "Don't know how to ~S ~S." 'expand binding))))


(define declare ((op &rest declaration-specifiers))
  (%expand-special-form op declaration-specifiers))

(define shadow ((op &rest binding-names))
  (%expand-special-form op binding-names))

(define progn ((op &body forms))
  (%expand-special-form op forms))

(define (or variable
            cl:function
            ignore
            ignorable
            special
            dynamic-extent
            inline
            notinline)
    ((op specification) form)
  (%expand-v/f-spec `(,op ,specification) form))

(define the ((op type-specifier specification) form)
  (%expand-v/f-spec `(,op ,type-specifier ,specification) form))

(define values ((op &rest specifications) form)
  (%expand-v/f-spec `(,op ,@specifications) form))

(define :destructuring ((op pattern) form)
  #+nil(%expand-v/f-spec `(,op ,pattern) form)
  (list `(,op ,pattern ,form)))

(define :accessors ((op specifications) form)
  (list `(,op ,specifications ,form)))

(define :slots ((op specifications) form)
  (list `(,op ,specifications ,form)))


(defun %expand-special-form (name specifications &optional forms)
  (check-type forms null)
  (list (cons name specifications)))

(defun %expand-v/f-spec (specification form)
  (flet ((extract-decls (analyzed)
	   (values (butlast analyzed)
		   (car (last analyzed))))
	 (conc (analyzed declarations)
	   (cons analyzed
                 (when declarations
                   (list `(declare ,@declarations))))))
    (let ((analyzed (%analyze-v/f-spec specification)))
      (case (length analyzed)
	(0 (list `(progn ,form)))
	(1 (multiple-value-bind (analyzed declarations)
	       (extract-decls (first analyzed))
	     (conc (append analyzed (list form))
                   declarations)))
	(t (let (declarationss analyzeds)
	     (dolist (analyzed analyzed
		      (conc `(:bindings ,(nreverse analyzeds)
					,form)
                            (apply #'append
				   (nreverse declarationss))))
	       (multiple-value-bind (analyzed declarations)
		   (extract-decls analyzed)
		 (push analyzed analyzeds)
		 (push declarations declarationss)))))))))

(defun %analyze-v/f-spec (spec)
  (case (let ((count 0))
	  (labels ((recurse (tree)
		     (cond ((consp tree)
			    (mapc #'recurse tree))
			   ((eq tree 'values)
			    (incf count)))))
	    (recurse spec))
	  count)
    (0)
    (1 (setf spec (bubble-op-up:bubble-operator-upwards 'values spec)))
    (t (error "The symbol ~S was found more than once in ~S."
	      'values
	      spec)))
  (let ((undeclared
	 (load-time-value (gensym (string "undeclared")))))
    (labels
	((var-spec (kind name declare-flags type-decl)
	   (list kind
		 name
		 (let ((declarations
			(mapcar
			 (ecase kind
			   (variable
			    (lambda (flag)
			      (case flag
				((inline notinline)
				 (error "Attempted to declare ~
                                         variable ~S ~S.~@
                                         Only functions ~
                                         can be declared ~:*~S."
					name flag))
				(t (list flag name)))))
			   (cl:function
			    (lambda (flag)
			     (list flag
				   (ecase flag
				     ((ignore ignorable dynamic-extent)
				      `#',name)
				     ((inline notinline)
				      name))))))
			 (nreverse declare-flags))))
		   (if (eq type-decl undeclared)
		       declarations
		       (cons (list (ecase kind
				     (variable 'type)
				     (cl:function 'ftype))
				   type-decl
				   name)
			     declarations)))))
	 (analyze (kind spec declare-flags type-decl)
	   (etypecase spec
	     (symbol
	      (if kind
		  (var-spec kind spec declare-flags type-decl)
		  (analyze 'variable spec declare-flags type-decl)))
	     ((cons (eql setf))
	      (cond ((not (symbolp (second spec)))
		     (error "Second part of SETF function name, ~
                             ~S, is not a symbol."
			    spec))
		    ((not kind)
		     (error "~A should be ~A."
			    (prin1-to-string spec)
			    (prin1-to-string `#',spec)))
		    ((eq kind 'variable)
		     (error "~S is a function name, not a variable name."
			    spec))
		    ((eq kind 'cl:function)
		     (var-spec kind spec declare-flags type-decl))))
	     (cons
	      (let ((operator (first spec)))
		(ecase operator
		  (:destructuring
		   (error "Sorry, ~S ~S is only ~
                           implemented at \"top-level\" at this time."
                          'bind :destructuring))
		  (the (analyze
			kind
			(third spec)
			declare-flags
			(if (eq type-decl undeclared)
			    (second spec)
			    (flet ((normalize (type)
				     (if (typep type '(cons (eql and)))
					 (cdr type)
					 (list type))))
			      `(and ,@(normalize type-decl)
				    ,@(normalize (second spec)))))))
		  ((variable cl:function)
		   (if kind
		       (if (eq operator kind)
			   (error "Duplicate ~S kind declaration."
				  kind)
			   (error "Contradictory ~S and ~S kind declarations."
				  kind operator))
		       (analyze operator
				(second spec)
				declare-flags
				type-decl)))
		  ((ignore ignorable special dynamic-extent inline notinline)
		   (check-type (cddr spec) null)
		   (analyze kind
			    (second spec)
			    (cons operator declare-flags)
			    type-decl))))))))
      (mapcar (lambda (spec)
		(analyze nil spec nil undeclared))
	      (if (typep spec '(cons (eql values)))
		  (cdr spec)
		  (list spec))))))
