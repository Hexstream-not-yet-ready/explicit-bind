(in-package #:explicit-bind)

(define-explicit-bind-expander declare ((op &rest declaration-specifiers))
  (%expand-special-form op declaration-specifiers))

(define-explicit-bind-expander shadow ((op &rest binding-names))
  (%expand-special-form op binding-names))

(define-explicit-bind-expander progn ((op &body forms))
  (%expand-special-form op forms))

(define-explicit-bind-expander (or variable
			  function
			  ignore
			  ignorable
			  special
			  dynamic-extent
			  inline
			  notinline)
    ((op specification) form)
  (%expand-v/f-spec `(,op ,specification) form))

(define-explicit-bind-expander the ((op type-specifier specification) form)
  (%expand-v/f-spec `(,op ,type-specifier ,specification) form))

(define-explicit-bind-expander values ((op &rest specifications) form)
  (%expand-v/f-spec `(,op ,@specifications) form))

(define-explicit-bind-expander :destructuring ((op pattern) form)
  #+nil(%expand-v/f-spec `(,op ,pattern) form)
  (list `(,op ,pattern ,form)))

(define-explicit-bind-expander :accessors ((op specifications) form)
  (list `(,op ,specifications ,form)))

(define-explicit-bind-expander :slots ((op specifications) form)
  (list `(,op ,specifications ,form)))


(defun %expand-special-form (name specifications &optional forms)
  (check-type forms null)
  (list (cons name specifications)))

(defun %expand-v/f-spec (specification form)
  (flet ((extract-decls (analyzed)
	   (values (butlast analyzed)
		   (car (last analyzed))))
	 (conc (declarations analyzed)
	   (nconc (when declarations
		    (list `(declare ,@declarations)))
		  (list analyzed))))
    (let ((analyzed (%analyze-v/f-spec specification)))
      (case (length analyzed)
	(0 (list `(progn ,form)))
	(1 (multiple-value-bind (analyzed declarations)
	       (extract-decls (first analyzed))
	     (conc declarations
		   `(:binding ,analyzed ,form))))
	(t (let (declarationss analyzeds)
	     (dolist (analyzed analyzed
		      (conc (apply #'append
				   (nreverse declarationss))
			    `(:bindings ,(nreverse analyzeds)
					,form)))
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
    (1 (setf spec (bubble-expand 'values spec)))
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
			   (function
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
				     (function 'ftype))
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
		    ((eq kind 'function)
		     (var-spec kind spec declare-flags type-decl))))
	     (cons
	      (let ((operator (first spec)))
		(ecase operator
		  (:destructuring
		   (error "Sorry, EXPLICIT-BIND :DESTRUCTURING is only ~
                           implemented at \"top-level\" at this time."))
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
		  ((variable function)
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
