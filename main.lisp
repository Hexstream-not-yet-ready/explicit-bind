(in-package #:explicit-bind)

(defun %merge (binding body)
  (etypecase (first binding)
    ((eql declare)
     (cons binding body))
    ((eql shadow)
     `(with-shadowed-bindings ,(rest binding)
	,@body))
    ((or symbol cons)
     (flet ((process (var-spec form)
	      (list
	       (destructuring-bind (kind name declarations) var-spec
		 (when declarations
		   (setf body (cons `(declare ,@declarations)
				    body)))
		 (ecase kind
		   (variable
		    `(let ((,name ,form))
		       ,@body))
		   (function
		    `(flet* ((,name ,form))
		       ,@body)))))))
       (destructuring-bind (vars form) binding
	 (when (typep form '(cons (eql declare)))
	   (error "Misplaced DECLARE expression ~S ~
                   as form for ~S." form vars))
	 (let ((analyzed (%analyze-binding vars)))
	   (case (length analyzed)
	     (0 (list form))
	     (1 (process (first analyzed) form))
	     (t (let* ((names (mapcar #'second analyzed))
		       (gensymed (mapcar #'%gensymed-function-name names)))
		  (list `(multiple-value-bind ,gensymed ,form
			   ,@(mapcan #'process analyzed names))))))))))))

(defmacro bind (bindings &body body)
  (let ((body (reduce #'%merge bindings
		      :from-end t
		      :initial-value body)))
    (if (typep (first body) '(cons (eql declare)))
	(cons 'locally body)
	(first body))))
