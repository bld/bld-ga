(defpackage :bld-ga
  (:use :common-lisp :bld-utils :alexandria)
  (:import-from :bld-linalg jacobi)
  (:shadowing-import-from :bld-gen
			  + - * / expt
			  sin cos tan
			  atan asin acos
			  sinh cosh tanh 
			  asinh acosh atanh
			  log exp sqrt abs
			  min max signum)
  (:import-from :bld-gen defmeth2 defmeth1)
  (:export :metric
	   :make-metric
	   :genrevtable
	   :g
	   :defg
	   :coef
	   :metric
	   :dimension
	   :size
	   :revtable
	   :bitmap
	   :unitvectors
	   :defgfun
	   :gref
	   :gset
	   :w/g
	   :loopg
	   :collectg
	   :ong
	   :newg
	   :w/newg
	   :mapcg
	   :mapg
	   :cpg
	   :w/cpg
	   :makeg
	   :gradeb
	   :grade
	   :grades
	   :graden
	   :bitmaps
	   :bitmap-part
	   :ga-coef+
	   :*o2
	   :*o
	   :*o3
	   :*g2
	   :*g
	   :*g3
	   :*i2
	   :*i
	   :*i3
	   :*c2
	   :*c
	   :*c3
	   :scalar
	   :*s2
	   :*s
	   :*s3
	   :revg
	   :invv
	   :refl
	   :rot
	   :spin
	   :normr2
	   :normr
	   :norme2
	   :norme
	   :norminf
	   :pseudoscalar
	   :dual
	   :unitg
	   :recipbv
	   :recipbvs
	   :oneg
	   :exptg
	   :square
	   :cube
	   :expbv
	   :rotor
	   :zerogp
	   :factormv
	   :linearlambda
	   :expand-mv-factors
	   :determinant
	   :basisi
	   :basis
	   :adjointvfun
	   :adjointlambda
	   :inverselambda))
