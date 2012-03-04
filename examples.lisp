(in-package #:explicit-bind)

#+nil
(bind ((#'foo #'identity)
       ((values #'remove-nil #'remove-t)
	(values-list
	 (mapcar (lambda (thing)
		   (lambda (sequence)
		     (remove (foo thing) sequence)))
		 '(nil t)))))
  (values (remove-t (remove-nil '(a b nil c t d)))
	  (remove-nil '(1 2 nil t nil 3))))

#+nil
(bind (((declare (optimize speed)))
       ((the (and really really) (ignore (ignorable (the (and nice like) (special (dynamic-extent (inline (notinline #'var))))))))
	value)))

#+nil
(bind (((values #'a b #'c d) (values #'+ 'b #'- 'd)))
  (values #'a b #'c d))
