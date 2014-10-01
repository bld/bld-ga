(in-package :bld-ga)

;;; Grades

(defmethod gradeb ((b integer))
  "Grade of a basis"
  (logcount b))

(defmethod grade ((g g))
  "Grade of GA object"
  (let (gr)
    (ong b c g
      (setq gr (if gr (max gr (gradeb b))
		   (gradeb b))))
    gr))

(defmethod grades ((g g))
  "Return list of all grades present in GA object"
  (remove-duplicates
   (loopg b c g
      unless (numberzerop c)
      collect (gradeb b))))

(defmethod graden ((g g) (n integer))
  "Return nth grade portion of GA object"
  (mapg #'(lambda (b c)
	    (if (= (gradeb b) n)
		c
		0))
	g))

(defmethod bitmaps ((g g))
  "Basis bitmaps present in GA object"
  (loopg b c g
     unless (numberzerop c)
     collect b))

(defmethod bitmap-part ((g g) bitmap)
  "Return portion of GA object that corresponds to given bitmap"
  (mapg #'(lambda (b c)
	    (if (find b bitmap)
		c
		0))
	g))

;;; Arithmetic (addition, subtractions, scalar multiplication)

;;; Addition

(defmethod ga-coef+ ((g g) (b integer) c)
  "Add scalar C to GA object's coefficient corresponfing to basis blade bitmap B"
  (mapg #'(lambda (bi ci)
	    (if (= bi b)
		(+ ci c)
		ci))
	g))

(defmeth2 + ((g g) (s t))
  (ga-coef+ g 0 s))

(defmeth2 + ((s t) (g g))
  (ga-coef+ g 0 s))

(defmeth2 + ((g1 g) (g2 g))
  (mapcg #'(lambda (c1 c2)
	     (if (and (numberzerop c1) (numberzerop c2))
		 0
		 (+ c1 c2)))
	 g1 g2))

;;; Subtraction

(defmeth2 - ((g g) (s t))
  (+ g (- s)))

(defmeth2 - ((s t) (g g))
  (+ s (- g)))

(defmeth2 - ((g1 g) (g2 g))
  (assert (typep g1 (type-of g2)))
  (mapcg #'(lambda (c1 c2)
	     (if (and (numberzerop c1) (numberzerop c2))		      
		 0
		 (- c1 c2)))
	 g1
	 g2))

;;; Scalar multiplication

(defmeth2 * ((g g) (s t))
  (mapcg #'(lambda (c) (* c s)) g))

(defmeth2 * ((s t) (g g))
  (mapcg #'(lambda (c) (* s c)) g))

(defmeth2 * ((g g) (s rational))
  (mapcg #'(lambda (c) (* c s)) g))

(defmeth2 * ((s rational) (g g))
  (mapcg #'(lambda (c) (* s c)) g))

;;; Scalar division

(defmeth2 / ((g g) (s t))
  (mapcg #'(lambda (c) (/ c s)) g))

;;; Negation

(defmeth1 - ((g g))
  (* g -1))

;;; Geometric Multiplication

(defun reordersign (a b)
  "Count # of swaps to reach canonical form for geometric product of 2 basis blades represented, in binary, as integers
e.g. e13 v e31, e123 v e231 and return 1 if even or -1 if odd"
  (if (evenp
       (let ((ai (ash a -1))
	     (sum 0))
	 (loop until (zerop ai)
	    do (incf sum (logcount (logand ai b)))
	      (setq ai (ash ai -1)))
	 sum))
      1
      -1))

(defun pairsign (b1 b2 m)
  "Sign change from removing pairs from geometric product of 2 bases with orthogonal signature"
  (loop with pairsign = 1
     for meet = (logand b1 b2) then (ash meet -1)
     for sigi across m
     until (zerop meet)
     unless (zerop (logand meet 1)) do (setq pairsign (* pairsign sigi))
     finally (return pairsign)))

(defmacro defgpe (name euclidean-bitmap-fn doc)
  "Define derived geometric product function on a Euclidean basis given name, euclidean function of bitmaps, and documentation string"
  `(defmethod ,name ((g1 g) (g2 g))
     ,doc
     (w/newg g3 g1
       (ong b1 c1 g1
	 (ong b2 c2 g2
	   (multiple-value-bind (b3 sign) (,euclidean-bitmap-fn b1 b2)
	     (setq g3 (ga-coef+ g3 b3 (* sign (* c1 c2))))))))))

(defmacro defgpo (name orthogonal-bitmap-fn doc)
  "Define a derived geometric product function on an orthogonal basis given name, orthogonal function of bitmaps & metric, and documentation string"
  `(defmethod ,name ((g1 g) (g2 g) (m vector))
     ,doc
     (w/newg g3 g1
       (ong b1 c1 g1
	 (ong b2 c2 g2
	   (multiple-value-bind (b3 sign) (,orthogonal-bitmap-fn b1 b2 m)
	     (setq g3 (ga-coef+ g3 b3 (* sign (* c1 c2))))))))))

;; Bitmap products (Euclidean & orthogonal)

(defun *gbe (b1 b2)
  "Geometric product of basis blade bitmaps in Euclidean space"
  (values (logxor b1 b2)
	  (reordersign b1 b2)))

(defun *ob (b1 b2)
  "Outer product of basis blade bitmaps (Euclidean, orthogonal, and non-orthogonal spaces)"
  (if (not (zerop (logand b1 b2)))
      (values 0 0)
      (*gbe b1 b2)))

(defun *gbo (b1 b2 m)
  "Geometric product of basis blade bitmaps in orthogonal space with vector metric"
  (values (logxor b1 b2)
	  (* (reordersign b1 b2)
	     (pairsign b1 b2 m))))

(defun *ibe (b1 b2)
  "Inner product of basis blade bitmaps in Euclidean space"
  (multiple-value-bind (basis sign) (*gbe b1 b2)
    (if (= (gradeb basis) (cl:- (gradeb b2) (gradeb b1)))
	(values basis sign)
	(values 0 0))))

(defun *ibo (b1 b2 m)
  "Inner product of basis blade bitmaps in orthogonal space with vector metric"
  (multiple-value-bind (basis sign) (*gbo b1 b2 m)
    (if (= (gradeb basis) (cl:- (gradeb b2) (gradeb b1)))
	(values basis sign)
	(values 0 0))))

(defun *cbe (b1 b2)
  "Commutator product of basis blade bitmaps in Euclidean space"
  (let ((s1 (* (reordersign b1 b2)))
	(s2 (* (reordersign b2 b1)))
	(basis (logxor b1 b2)))
    (values basis (/ (- s1 s2) 2))))

(defun *cbo (b1 b2 m)
  "Commutator product of basis blade bitmaps in orthogonal space with vector metric"
  (let ((s1 (* (reordersign b1 b2) (pairsign b1 b2 m)))
	(s2 (* (reordersign b2 b1) (pairsign b2 b1 m)))
	(basis (logxor b1 b2)))
    (values basis (/ (- s1 s2) 2))))

;; Outer product

(defgpe *o2 *ob "Outer product of 2 GA objects (Euclidean, orthogonal, & non-orthogonal spaces)")
(defun *o (&rest args) "Outer product of GA objects (all spaces)" (reduce #'*o2 args))
(defmethod *o3 ((g1 g)(g2 g)(g3 g)) "Outer product of 3 GA objects (all spaces)" (*o g1 g2 g3))

;; Non-orthogonal products

(defun transform (class bitmap scale m dim)
  "Transform a basis blade, given bitmap and scale, to a multivector using tranform matrix"
  (let ((al (makeg class :s scale)))
    (loop for b = bitmap then (ash b -1)
       for i below dim
       until (zerop b)
       do (unless (zerop (logand b 1))
	    (w/g tmp class
	      (loop for j below dim
		 for mji = (aref m j i)
		 do (unless (zerop mji)
		      (ong alb alc al
			(setq tmp (+ tmp (*o2 (makeg class (elt (basisbladekeys tmp) alb) alc)
					      (makeg class (elt (basisbladekeys tmp) (ash 1 j)) mji)))))))
	      (setq al tmp))))
    al))

(defun toeigenbasis (a m)
  "Transform non-orthogonal GA object to from metric to eigen basis"
  (w/newg out a
    (ong b c a
      (setq out (+ out (transform (type-of a) b c (inveigmatrix m) (dimension a)))))))

(defun tometricbasis (a m)
  "Transform non-orthogonal GA object back to metric from eigen basis"
  (w/newg out a
    (ong b c a
      (setq out (+ out (transform (type-of a) b c (eigmatrix m) (dimension a)))))))

(defmacro defgpno (name ofn doc)
  "Define derived geometric product on non-orthogonal basis given name, corresponding orthogonal function of MVs & vector metric, and documentation string"
  `(defmethod ,name ((g1 g) (g2 g) (m metric))
     ,doc
     (tometricbasis (,ofn (toeigenbasis g1 m) (toeigenbasis g2 m) (eigenmetric m)) m)))

;; Define other geometric products

(defmacro defgp (name euclid-fn orth-fn nonorth-fn doc)
  "Define derived geometric product given Euclidean, orthogonal, and non-orthogonal functions."
  `(defmethod ,name ((g1 g) (g2 g))
     ,doc
     (typecase (metric g1)
       (null (,euclid-fn g1 g2))
       (vector (,orth-fn g1 g2 (metric g1)))
       (metric (,nonorth-fn g1 g2 (metric g1))))))

(defgpe *g2e *gbe "Geometric product with Euclidean metric")
(defgpo *g2o *gbo "Geometric product with orthogonal metric")
(defgpno *g2no *g2o "Geometric product with non-orthogonal metric")
(defgp *g2 *g2e *g2o *g2no "Geometric product of 2 GA objects")
(defun *g (&rest args) "Geometric product" (reduce #'*g2 args))
(defmethod *g3 ((g1 g)(g2 g)(g3 g)) "Geometric product of 3 GA objects" (*g g1 g2 g3))
(defgpe *i2e *ibe "Inner contraction product with Euclidean basis")
(defgpo *i2o *ibo "Inner contraction product with orthogonal basis")
(defgpno *i2no *i2o "Inner contraction product with non-orthogonal basis")
(defgp *i2 *i2e *i2o *i2no "Inner contraction product")
(defun *i (&rest args) (reduce #'*i2 args))
(defmethod *i3 ((g1 g)(g2 g)(g3 g)) "Inner contraction product of 3 GA objects" (*i g1 g2 g3))
(defgpe *c2e *cbe "Commutator product with Euclidean basis")
(defgpo *c2o *cbo "Commutator product with orthogonal basis")
(defgpno *c2no *c2o "Commutator product with non-orthogonal basis")
(defgp *c2 *c2e *c2o *c2no "Commutator product")
(defun *c (&rest args) (reduce #'*c2 args))
(defmethod *c3 ((g1 g)(g2 g)(g3 g)) "Commutator product of 3 GA objects" (*c g1 g2 g3))

(defmethod scalar ((g g))
  "Scalar part of GA object"
  (slot-value g (intern "S")))

(defmethod *s2 ((g1 g) (g2 g))
  "Scalar product of 2 GA objects"
  (scalar (*g2 g1 g2)))
(defmethod *s2 ((g1 g) g2)
  (*gs g1 g2))
(defmethod *s2 (g1 (g2 g))
  (*gs g2 g1))
(defmethod *s2 (g1 g2)
  (* g1 g2))
(defun *s (&rest args) "Scalar product" (reduce #'*s2 args))
(defmethod *s3 ((g1 g)(g2 g)(g3 g)) "Scalar product of 3 GA objects" (*s g1 g2 g3))

;; Reverse

(defmethod revg ((g g))
  "Reverse of GA object"
  (mapg #'(lambda (b c) (* c (aref (revtable g) b))) g))

;; Versor inverse

(defmethod invv ((g g))
  "Inverse of a versor"
  (/ (revg g) (scalar (*g2 g (revg g)))))

;; Reflection/rotation

(defmethod reflectline ((g g) (n g))
  "Reflect GA object in a line defined by a vector"
  (*g n g (invv n)))

(defmethod reflectplane ((g g) (n g))
  "Reflect a GA object in plane defined by normal vector"
  (- (reflectline g n)))

(defmethod rotateg ((g g) (r g))
  "Rotate GA object by rotor (normalized)"
  (*g3 r g (invv r)))

(defmethod spin ((g g) (s g))
  "Spin a GA object by a spinor (not normalized)"
  (*g3 s g (revg s)))

;; Norms, unit GA objects

(defmethod normr2 ((g g))
  "Reverse norm squared"
  (scalar (*g2 g (revg g))))

(defmethod normr ((g g))
  "Reverse norm"
  (let ((nr2g (normr2 g)))
    (* (signum nr2g) (sqrt (abs nr2g)))))

(defmethod norme2 ((g g))
  "Euclidean norm squared"
  (scalar (*g2e g (revg g))))

(defmethod norme ((g g))
  "Euclidean norm"
  (sqrt (norme2 g)))

(defmethod norminf ((g g))
  "Infinity norm"
  (apply #'max
	 (collectg b c g (abs c))))

(defmethod pseudoscalar ((g g))
  "Pseudoscalar of given GA object"
  (w/newg i g
    (gset i (1- (size i)) 1)))

(defmethod dual ((g g))
  "Dual"
  (*i2 g (invv (pseudoscalar g))))

(defmethod unitg ((g g))
  "Unitize"
  (/ g (norme g)))

(defun recipbv (j &rest bvs)
  "jth reciprocal basis vector"
  (* (*g (apply #'*o (remove-nth j bvs))
	    (apply #'*o bvs))
       (expt -1 (1- j))))

(defun recipbvs (&rest bvs)
  "Given a list of basis vectors, generate reciprocal basis"
  (loop for k below (length bvs)
     collect (apply #'recipbv k bvs)))

(defmethod oneg ((g g))
  "1 GA object"
  (w/newg one g
    (gset one 0 1)))

(defmethod square ((g g))
  "Square GA object"
  (*g2 g g))

(defmethod exptg ((g g) (n integer))
  "GA object multiplied n times"
  (cond ((zerop n) (oneg g))
	((evenp n) (square (exptg g (cl:/ n 2))))
	(t (*g2 g (exptg g (1- n))))))

(defmethod cube ((g g))
  "Cube GA object"
  (exptg g 3))

;; Tests of GA object type
(defmethod scalarp ((g g))
  "Test if GA object is a scalar"
  (and (typep g 'g)
       (let ((gg (grade g)))
	 (or (null gg)
	     (zerop gg)))))

;; Exponentials & rotors: numeric only
(defmethod expbs ((b g))
  "Exponential of bivector with scalar square"
  (let ((b2 (*g2 b b)))
    (when (scalarp b2)
      (let ((b2s (scalar b2)))
	(cond ((< b2s 0)
	       (let ((a (sqrt (- b2s))))
		 (+ (* b (/ (sin a) a)) (cos a))))
	      ((zerop b2s)
	       (+ b 1))
	      ((> b2s 0)
	       (let ((a (sqrt b2s)))
		 (+ (* b (/ (sinh a) a)) (cosh a)))))))))
(defmethod expbv ((b g) &optional (order 9))
  "Bivector exponential"
  (let* ((eps 1d-7)
	 (b2 (*g2 b b))
	 (b2s (scalar b2)))
    (if (< (- (norme2 b2) (* b2s b2s)) eps) ; scalar norm
	(expbs b)
	;; otherwise run Taylor expansion
	(let ((bout (makeg (type-of b) #b0 1))
	      (maxbb (position (apply #'max b) b))
	      (scale 1))
	  ;; Scale below 1
	  (when (> maxbb 1) (= scale (ash scale 1)))
	  (loop until (zerop maxbb)
	       do (setq maxbb (ash maxbb -1))
	       (setq scale (ash scale 1)))
	  ;; Taylor approximation
	  (let ((scaled (*gs b (/ scale)))
		(tmp (makeg (type-of b) #b0 1)))
	    (loop for i below order
	       do (setq tmp (* (*g2 tmp scaled) (/ i)))
		 (+ bout tmp)))
	  ;; Undo scaling
	  (loop while (> scale 1)
	       do (setq bout (*g2 bout bout))
	       (setq scale (ash scale -1)))
	  bout))))

(defmethod rotor ((b g) a)
  "Create a rotor given a bivector (rotation plane) and angle"
  ;;(expbv (* (unitg b) (/ a -2))))
  (+ (* (unitg b) (- (sin (/ a 2))))
     (cos (/ a 2))))

;;; Test functions

(defmethod zerogp ((g g))
  "Test if GA object is zero"
  (null (grade g)))
