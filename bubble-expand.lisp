(in-package #:explicit-bind)

;; Horrible implementation I wrote as a newb...
(defun cartesian-product (branches)
  (let ((permutation-count (apply #'* (mapcar #'length branches))))
    (loop for permutation below permutation-count
	  collect (loop for branch in branches
			for length = (length branch)
			for permutations-so-far = length then (* permutations-so-far
									     length)
			collect (nth (mod (floor permutation
						 (floor permutation-count
							permutations-so-far))
					  length)
				     branch)))))

(defun bubble-expand (operator form)
  "Bubble OPERATOR upwards in FORM, multiplexing all possible branches by way of cartesian join.

Example:
\(bubble-expand 'and
		'(this (is a (and simple trivial) example to show) how it works))
==>
\(AND (THIS (IS A SIMPLE EXAMPLE TO SHOW) HOW IT WORKS)
      (THIS (IS A TRIVIAL EXAMPLE TO SHOW) HOW IT WORKS))"
  (labels ((bubbling-form-p (form)
	     (and (consp form) (eq (first form) operator)))
	   (find-bubbling-form (form)
	     (if (consp form)
		 (if (bubbling-form-p form) form (find-if #'find-bubbling-form form))))
	   (recurse (form)
	     (if (consp form)
		 (values-list (if (bubbling-form-p form)
				  (mapcan (lambda (value)
					    (multiple-value-list (recurse value)))
					  (cdr form))
				  (mapcar (lambda (branch)
					    (cons (car form) branch))
					  (cartesian-product
					   (mapcar (lambda (value)
						     (multiple-value-list
							 (recurse value)))
						   (cdr form))))))
		 (values form))))
    (if (bubbling-form-p form)
	(if (find-if #'find-bubbling-form (cdr form))
	    (cons operator
		  (mapcan (lambda (form)
			    (multiple-value-list (recurse form)))
			  (cdr form)))
	    form)
	(if (find-bubbling-form form)
	    (let ((result (multiple-value-list (recurse form))))
	      (if (cdr result)
		  (cons operator result)
		  (car result)))
	    form))))
