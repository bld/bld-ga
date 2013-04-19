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

(test grades
  (is (null (grades (e2))))
  (is (equal (grades (e2 :s 1 :e1 2 :e2 3 :e1e2 4))
	     (list 0 1 2))))

(test graden
  (let ((g (e2 :s 1 :e1 2 :e2 3 :e1e2 4)))
    (is (equalg (graden g 0) (e2 :s 1)))
    (is (equalg (graden g 1) (e2 :e1 2 :e2 3)))
    (is (equalg (graden g 2) (e2 :e1e2 4)))))

(test bitmaps
  (is (null (bitmaps (e2))))
  (is (equal (bitmaps (e2 :s 1 :e1 2 :e2 3 :e1e2 4))
	     (list #b0 #b1 #b10 #b11))))

(test bitmap-part
  (is (equalg (bitmap-part (e2 :s 1) (list #b1 #b10 #b11)) (e2)))
  (let ((g (e2 :s 1 :e1 2 :e2 3 :e1e2 4)))
    (is (equalg (bitmap-part g (list #b0 #b1 #b10 #b11)) g))))
    
(test +
  (is (equalg (+ (e2) (e2)) (e2)))
  (is (equalg (+ (e2) 1) (e2 :s 1)))
  (is (equalg (+ (e2 :s 1 :e1 2)
		 (e2 :e2 3 :e1e2 4))
	      (e2 :s 1 :e1 2 :e2 3 :e1e2 4))))

(test -
  (let ((g (e2 :s 1 :e1 2 :e2 3 :e1e2 4)))
    (is (equalg (- (e2)) (e2)))
    (is (equalg (- g)
		(e2 :s -1 :e1 -2 :e2 -3 :e1e2 -4)))
    (is (equalg (- g g) (e2)))
    (is (equalg (- g 1) (e2 :e1 2 :e2 3 :e1e2 4)))
    (is (equalg (- 1 g) (e2 :e1 -2 :e2 -3 :e1e2 -4)))))

(test *
  (let ((g (e2 :s 1 :e1 2 :e2 3 :e1e2 4)))
    (is (equalg (* g 2) (e2 :s 2 :e1 4 :e2 6 :e1e2 8)))
    (is (equalg (* 2 g) (e2 :s 2 :e1 4 :e2 6 :e1e2 8)))
    (is (equalg (* g 0) (e2)))))

(test /
  (let ((g (e2 :s 2 :e1 4 :e2 6 :e1e2 8)))
    (is (equalg (/ g 2) (e2 :s 1 :e1 2 :e2 3 :e1e2 4)))))

(test *o
  (is (equalg (*o (e2) (e2)) (e2)))
  (is (equalg (*o (e2 :s 2) (e2 :s 3)) (e2 :s 6)))
  (is (equalg (*o (e2 :e1 2) (e2 :e2 3)) (e2 :e1e2 6)))
  (is (equalg (*o (e2 :e2 2) (e2 :e1 3)) (e2 :e1e2 -6)))
  (is (equalg (*o (e2 :e1 1) (e2 :e1 1)) (e2)))
  (is (equalg (*o (e2 :e1 1) (e2 :e1e2 1)) (e2))))

(test *g
  ;; E2
  (is (equalg (*g (e2) (e2)) (e2)))
  (is (equalg (*g (e2 :s 1) (e2 :s 1)) (e2 :s 1)))
  (is (equalg (*g (e2 :s 1) (e2 :e1 1)) (e2 :e1 1)))
  (is (equalg (*g (e2 :e1 1) (e2 :e1 1)) (e2 :s 1)))
  (is (equalg (*g (e2 :e1 1) (e2 :e2 1)) (e2 :e1e2 1)))
  (is (equalg (*g (e2 :e1 1) (e2 :e1e2 1)) (e2 :e2 1)))
  (is (equalg (*g (e2 :e1e2 1) (e2 :e1e2 1)) (e2 :s -1)))
  ;; C3
  (is (equalg (*g (c3 :no 1) (c3 :ni 1)) (c3 :s -1 :noni 1) 1d-6))
  (is (equalg (*g (c3 :no 1) (c3 :no 1)) (c3)))
  (is (equalg (*g (c3 :ni 1) (c3 :ni 1)) (c3))))

(test *i
  ;; E2
  (is (equalg (*i (e2 :s 1) (e2 :s 1)) (e2 :s 1)))
  (is (equalg (*i (e2 :s 2) (e2 :e1 1)) (e2 :e1 2)))
  (is (equalg (*i (e2 :e1 1) (e2 :s 2)) (e2)))
  (is (equalg (*i (e2 :e1 1) (e2 :e1 1)) (e2 :s 1)))
  (is (equalg (*i (e2 :e1 1) (e2 :e2 1)) (e2)))
  (is (equalg (*i (e2 :e1 1) (e2 :e1e2 1)) (e2 :e2 1)))
  (is (equalg (*i (e2 :e1e2 1) (e2 :e1 1)) (e2)))
  (is (equalg (*i (e2 :e2 1) (e2 :e1e2 1)) (e2 :e1 -1)))
  (is (equalg (*i (e2 :e1e2 1) (e2 :e1e2 1)) (e2 :s -1)))
  ;; E3
  (is (equalg (*i (e3 :e1 1) (e3 :e1e2e3 1)) (e3 :e2e3 1)))
  (is (equalg (*i (e3 :e2 1) (e3 :e1e2e3 1)) (e3 :e1e3 -1)))
  (is (equalg (*i (e3 :e1e2 1) (e3 :e1e2e3 1)) (e3 :e3 -1)))
  (is (equalg (*i (e3 :e1e2e3 1) (e3 :e1e2e3 1)) (e3 :s -1)))
  ;; C3
  (is (equalg (*i (c3 :no 1) (c3 :e1 1)) (c3)))
  (is (equalg (*i (c3 :no 1) (c3 :ni 1)) (c3 :s -1) 1d-6))
  (is (equalg (*i (c3 :ni 1) (c3 :no 1)) (c3 :s -1) 1d-6))
  (is (equalg (*i (c3 :no 1) (c3 :no 1)) (c3)))
  (is (equalg (*i (c3 :ni 1) (c3 :ni 1)) (c3))))

(test *c
  (let ((a (e3 :s 1 :e1 2 :e2 3 :e3 4 :e1e2 5 :e2e3 6 :e1e3 7 :e1e2e3 8))
	(b (e3 :s 9 :e1 10 :e2 11 :e3 12 :e1e2 13 :e2e3 14 :e1e3 15 :e1e2e3 16)))
    (is (equalg (*c a b) (/ (- (*g a b) (*g b a)) 2)))))

(test scalar
  (is (= (scalar (e2 :s 1 :e1 2 :e2 3 :e1e2 4)) 1))
  (is (zerop (scalar (e2 :e1 2 :e2 3 :e1e2 4)))))

(test *s
  (let ((a (e2 :s 1 :e1 2 :e2 3 :e1e2 4))
	(b (e2 :s 5 :e1 6 :e2 7 :e1e2 8)))
    (is (= (*s a b) (+ (* 1 5) (* 2 6) (* 3 7) (- (* 4 8)))))))

(test revg
  (is (equalg (revg (e2 :s 1 :e1 2 :e2 3 :e1e2 4))
	      (e2 :s 1 :e1 2 :e2 3 :e1e2 -4))))

(test invv
  (let ((g (e3 :s 1 :e1e2 5 :e2e3 6 :e1e3 7)))
    (is (equalg (*g g (invv g)) (e3 :s 1)))))

(test reflectline
  (is (equalg (reflectline (e2 :e1 1 :e2 2) (e2 :e1 1)) (e2 :e1 1 :e2 -2))))

(test reflectplane
  (is (equalg (reflectplane (e2 :e1 1 :e2 2) (e2 :e1 1)) (e2 :e1 -1 :e2 2))))

(test rotor
  (is (equalg (rotor (e3 :e1e2 1 :e2e3 1 :e1e3 1) 0) (e3 :s 1)))
  (is (equalg (rotor (e3 :e1e2 1) (/ pi 2)) 
	      (e3 :s (cos (/ pi 4)) :e1e2 (- (sin (/ pi 4))))
	      1d-6))
  (is (equalg (rotor (e3 :e1e2 1 :e2e3 1 :e1e3 1) (/ pi 4))
	      (e3 :s (cos (/ pi 8)) 
		  :e1e2 (/ (- (sin (/ pi 8))) (sqrt 3))
		  :e2e3 (/ (- (sin (/ pi 8))) (sqrt 3))
		  :e1e3 (/ (- (sin (/ pi 8))) (sqrt 3))) 
	      1d-6)))

(test rotate
  (let ((g (e3 :e1 1 :e2 2 :e3 3)))
    (is (equalg (rotate g (rotor (e3 :e1e2 1 :e2e3 1 :e1e3 1) 0)) g))
    (is (equalg (rotate g (rotor (e3 :e1e2 1) (/ pi 2))) 
		(e3 :e1 -2 :e2 1 :e3 3) 
		1d-6))
    (is (equalg (rotate g (rotor (e3 :e1e2 1) (/ pi 4)))
		(e3 :e1 (/ (- 1 2) (sqrt 2d0))
		    :e2 (/ (+ 1 2) (sqrt 2))
		    :e3 3)
		1d-6))))

(test spin
  (let* ((g (e3 :e1 1 :e2 2 :e3 3))
	 (r 2)
	 (s (* (sqrt r) (rotor (e3 :e1e2 1 :e2e3 2 :e1e3 3) (/ pi 4)))))
    (is (equalg (spin g s) (* r (rotate g s)) 1d-6))))
    
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

(test zerogp)

;;; linear.lisp

(test factormv)

