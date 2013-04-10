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


