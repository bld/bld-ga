;; Linear functions
#|
Development notes:
Purpose is to define linear functions of multivectors and those functions determinants, adjoints, & inverses.
May want these as methods, so they can be specialized via BLD-GAGEN.
How to do this?
Need knowledge of the geometric algebra used, so the pseudoscalar and a unit vector basis can be used to calculate the determinant, adjoint, and inverse.
Within a linear function this can be done, because the GA object is the argument. How then to do this when the argument IS the function?
Maybe.... use closures for the basic function, but define a top-level method using it.
Annoying that methods aren't 1st class objects.
Really, need linear functions as 1st-class objects, so I can pass them around and operate on them.
Can I produce simplified versions after the fact, then, in BLD-GAGEN? Perhaps, via typecase within the functions?
I needed to make a special generation macro for GRADEN, because of the grade dependency.
Or not. The linear functions could just be closures. They all take only one argument, and map GA objects to ones of the same grade.
Inverse is proving problematic, because it is 
|#

(in-package :bld-ga)

(defun factormv (mv)
  "Return factorization of multivector as list of summed blades, and blades as a list of basis vectors"
  (flet ((factorblade (b c dimension class)
	   (loop for dim below dimension
	      for bvb = (expt 2 dim)
	      when (/= 0 (logand bvb b))
	      collect (makeg class bvb (expt c (/ (logcount b)))))))
    (loop for c across (coef mv)
       for b across (bitmap mv)
       if (and (not (equal 0 c))
	       (not (zerop b)))
       collect (factorblade b c (dimension mv) (type-of (graden mv 1)))
       else if (and (zerop b)
		    (not (zerop c)))
       collect (list c))))

(defun linearlambda (ga vlambda)
  "Return a linear function of multivectors given the geometric algebra and a linear function of vectors"
  (dlambda
   (:ga () ga)
   (:vlambda () vlambda)
   (t (mv)
      (cond
	((and (typep mv 'g) (eql (grade mv) 1)) ; vectors, grade 1, apply vector function
	 (funcall vlambda mv))
	((typep mv 'g) ; factor multivector, apply to each part, outer product blades, and sum
	 (apply #'+ (loop for factor in (factormv mv)
		       collect (apply #'*o (mapcar #'(lambda (v)
						       (if (typep v 'g)
							   (funcall vlambda v)
							   v)) factor)))))
	(t mv))))) ; otherwise (for scalars) just return

(defun expand-mv-factors (factors)
  "Expand factorized multivector"
  (apply #'+ (loop for factor in factors
		collect (apply #'*o factor))))

(defun determinant (lfun)
  "Determinant of a linear function acting on a pseudoscalar"
  (let ((i (pseudoscalar (make-instance (funcall lfun :ga)))))
    (scalar (*g (funcall lfun i) (revg i)))))

(defmethod basisi ((g g) (bvb number))
  "Generate one default basis vector given GA object and basis vector bitmap (e.g. #b1 #b10 #b100)"
  (graden (makeg (type-of g) bvb 1) 1))

(defmethod basis ((g g))
  "Generate default list of basis vectors given a GA object"
  (loop for i below (dimension g)
     collect (basisi g (expt 2 i))))

(defun adjointvfun (lfun &optional (basis (basis (make-instance (funcall lfun :ga)))))
  "Make adjoint vector function given linear function and optional list of basis vectors."
  (lambda (v)
    (apply #'+ (loop for bi in basis
		  for bir in (apply #'recipbvs basis)
		  collect (* (*s v (funcall (funcall lfun :vlambda) bi)) bir)))))

(defun adjointlambda (lfun &optional (basis (basis (make-instance (funcall lfun :ga)))))
  "Make adjoint linear function of multivectors"
  (linearlambda (funcall lfun :ga) (adjointvfun lfun basis)))

(defun inverselambda (lfun &optional (basis (basis (make-instance (funcall lfun :ga)))))
  "Make inverse linear function of given linear function and optional basis."
  (let* ((ga (funcall lfun :ga))
	 (i (pseudoscalar (make-instance ga)))
	 (ii (invv i))
	 (adj (adjointlambda lfun basis))
	 (invdet (/ (determinant lfun))))
    (linearlambda ga #'(lambda (v) (* invdet (*g i (funcall adj (*g ii v))))))))
