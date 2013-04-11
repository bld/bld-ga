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

(defg e2 2)

(defg e3 3)

(defg h3 4)

(defg c3 5 #2a((0 0 0 0 -1)
	       (0 1 0 0 0)
	       (0 0 1 0 0)
	       (0 0 0 1 0)
	       (-1 0 0 0 0)))

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

(test metric
  (is (null (make-metric nil)))
  (is (every #'= (make-metric #(1 1 1)) #(1 1 1)))
  (is (typep (metric (make-instance 'c3)) 'metric)))

