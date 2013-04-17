(defpackage :bld-ga-tests
  (:use :common-lisp :bld-ga :fiveam)
  (:shadowing-import-from :bld-gen
			  + - * / expt
			  sin cos tan
			  atan asin acos
			  sinh cosh tanh 
			  asinh acosh atanh
			  log exp sqrt abs
			  min max signum))

(in-package :bld-ga-tests)

(def-suite :bld-ga)

(in-suite :bld-ga)

(defun almost= (a b &optional (eps double-float-epsilon))
  (< (abs (- a b)) eps))

(defmethod equalg ((a g) (b g) &optional (eps double-float-epsilon))
  "Test if two GA objects are equal"
  (and (equal (type-of a) (type-of b))
       (every #'(lambda (a b) (almost= a b eps)) (coef a) (coef b))))

(defg e2 #(e1 e2))

(defg e3 #(e1 e2 e3))

(defg h3 #(e0 e1 e2 e3))

(defg c3 #(no e1 e2 e3 ni) #2a((0 0 0 0 -1)
			       (0 1 0 0 0)
			       (0 0 1 0 0)
			       (0 0 0 1 0)
			       (-1 0 0 0 0)))

;;; metric.lisp tests

(test metric
  (is (null (make-metric nil)))
  (is (every #'= (make-metric #(1 1 1)) #(1 1 1)))
  (is (typep (make-metric #2a((1 0 0)(0 1 0)(0 0 1))) 'metric)))

;;; mv.lisp tests

(def-fixture with-revtable (n)
  (let ((revtable (genrevtable n)))
    (&body)))

(test revtable
  (with-fixture with-revtable (3)
    (is (= (aref revtable #b0) 1))
    (is (= (aref revtable #b1) 1))
    (is (= (aref revtable #b10) 1))
    (is (= (aref revtable #b100) 1))
    (is (= (aref revtable #b11) -1))
    (is (= (aref revtable #b101) -1))
    (is (= (aref revtable #b110) -1))
    (is (= (aref revtable #b111) -1))))

(test g
  (is (every #'zerop (coef (e2))))
  (is (every #'zerop (coef (e3))))
  (is (every #'= (coef (e2 :s 1 :e1 2 :e2 3 :e1e2 4)) #(1 2 3 4)))
  (is (every #'= (coef (e3 :s 1 :e1 2 :e2 3 :e1e2 4 :e3 5 :e1e3 6 :e2e3 7 :e1e2e3 8))
	     #(1 2 3 4 5 6 7 8))))

(test gref
  (signals (error "GREF out of bounds didn't signal an error") (gref (e2) #b111))
  (is (= (gref (e2 :s 1) #b0) 1))
  (is (= (gref (e2 :s 1) #b1) 0)))

(test gset
  (is (let ((g (e2)))
	(gset g #b0 1)
	(= (gref g #b0) 1)))
  (is (let ((g (e2)))
	(setf (gref g #b0) 1)
	(= (gref g #b0) 1))))

(test w/g
  (is (typep (w/g g 'e2) 'e2))
  (is (= (gref (w/g g 'e2
		 (setf (gref g #b0) 1))
	       #b0) 1)))

(test loopg)

(test collectg)

(test ong)

(test newg)

(test w/newg)

(test mapcg)

(test cpg)

(test w/cpg)

(test makeg)

;;; ga.lisp tests

(test gradeb
  (is (zerop (gradeb #b0)))
  (is (= (gradeb #b1) 1))
  (is (= (gradeb #b10) 1))
  (is (= (gradeb #b101) 2)))

(test grade
  (is (null (grade (e2))))
  (is (= (grade (e2 :s 1)) 0))
  (is (= (grade (e2 :e1 1)) 1))
  (is (= (grade (e2 :e1e2 1)) 2)))

(test grades)

(test graden)

(test bitmaps)

(test bitmap-part)

(test ga-coef+)

(test +)

(test -)

(test *)

(test /)

(test *o)

(test *g)

(test *i)

(test *c)

(test scalar)

(test *s)

(test revg)

(test invv)

(test refl)

(test rot)

(test spin)

(test normr2)

(test normr)

(test norme2)

(test norme)

(test norminf)

(test pseudoscalar)

(test dual)

(test unitg)

(test recipbv)

(test recipbvs)

(test oneg)

(test exptg)

(test square)

(test cube)

(test expbv)

(test rotor)

(test zerogp)

;;; linear.lisp

(test factormv)

