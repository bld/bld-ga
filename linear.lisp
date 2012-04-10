;; Linear functions

(in-package :bld-ga)

(defun factorblade (b c dimension class)
  "Return a list of basis vectors whose outer product results in a basis blade of given basis bitmap, coefficient, and dimension. Also give vector class."
  (loop for dim below dimension
     for bvb = (expt 2 dim)
     when (/= 0 (logand bvb b))
     collect (graden (makeg class bvb (expt c (/ (logcount b)))) 1)))

(defun factormv (mv)
  "Return factorization of multivector"
  (loop for c across (coef mv)
     for b across (bitmap mv)
     if (and (not (zerop c))
	     (not (zerop b)))
     collect (factorblade b c (dimension mv) (type-of (graden mv 1)))
     else if (and (zerop b)
		  (not (zerop c)))
     collect (list c)))

(defun expand-mv-factors (factors)
  "Expand factorized multivector"
  (apply #'+ (loop for factor in factors
		collect (apply #'*o factor))))

(defun linearfun (fun mv)
  "Apply linear vector function to multivector"
  (apply #'+ (loop for factor in (factormv mv)
		collect (apply 
			 #'*o (mapcar 
			       #'(lambda (v) 
				   (if (typep v 'g) (funcall fun v) v)) factor)))))
