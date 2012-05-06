(in-package #:explicit-bind)

(defun %trace ()
  (trace eb-expander:expand eb-merger:merge eb-merger:merge1))

(defun %untrace ()
  (untrace eb-expander:expand eb-merger:merge eb-merger:merge1))

(defun %merge (binding body)
  (etypecase (first binding)
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
  (let* ((expanded (mapcan
                    (lambda (binding)
                      (copy-seq (eb-expander:expand binding)))
                    bindings))
         (body (eb-merger:merge expanded body)))
    (cond ((not body)
           nil)
          ((not (rest body))
           (first body))
          (t `(progn ,@body)))))
