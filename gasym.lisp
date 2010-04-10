(in-package :bld-ga)

(export '(gradeb grade grades graden gbc+ g2+ g+ g2- *gs /gs g- *o2 *o *g2 *g *i2 *i *c2 *c scalar *s2 *s revg invv refl rot spin normr2 normr norme2 norme))

;; Grades

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

;; Arithmetic (addition, subtractions, scalar multiplication)

(defmethod gbc+ ((g g) (b integer) c)
  "Add C to GA object's B coefficient"
  (mapg #'(lambda (bi ci)
	    (if (= bi b)
		(simp `(+ ,ci ,c))
		ci))
	g))

(defmethod g2+ ((g1 g) (g2 g))
  "Add two GA objects"
  (assert (typep g1 (type-of g2)))
  (mapcg #'(lambda (c1 c2)
	     (if (and (numberzerop c1) (numberzerop c2))
		 0
		 (simp `(+ ,c1 ,c2))))
	 g1
	 g2))

(defun g+ (&rest args)
  "Add a series of GA objects"
  (reduce #'g2+ args))

(defmethod g2- ((g1 g) (g2 g))
  "Subtract one GA object from another"
  (assert (typep g1 (type-of g2)))
  (mapcg #'(lambda (c1 c2)
	     (if (and (numberzerop c1) (numberzerop c2))		      
		 0
		 (simp `(+ ,c1 (- ,c2)))))
	 g1
	 g2))

(defmethod *gs ((g g) s)
  "Multiply GA object by a scalar"
  (mapcg #'(lambda (c) (simp `(* ,c ,s))) g))

(defmethod /gs ((g g) s)
  "Divide GA object by a scalar"
  (mapcg #'(lambda (c) (simp `(* ,c (expt ,s -1)))) g))

(defun g- (arg1 &rest args)
  "If 1 arg, negate. Otherwise, subtract the rest of the arguments from 1st."
  (if args
      (reduce #'g2- (cons arg1 args))
      (*gs arg1 -1)))

;; Multiplication

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

(defmacro defgpe (name ebfn doc)
  "Define derived geometric product function on a Euclidean basis given name, euclidean function of bitmaps, and documentation string"
  `(defmethod ,name ((g1 g) (g2 g))
     ,doc
     (w/newg g3 g1
       (ong b1 c1 g1
	 (ong b2 c2 g2
	   (multiple-value-bind (b3 sign) (,ebfn b1 b2)
	     (setq g3 (gbc+ g3 b3 (simp `(* ,sign ,c1 ,c2))))))))))

(defmacro defgpo (name obfn doc)
  "Define a derived geometric product function on an orthogonal basis given name, orthogonal function of bitmaps & metric, and documentation string"
  `(defmethod ,name ((g1 g) (g2 g) (m vector))
     ,doc
     (w/newg g3 g1
       (ong b1 c1 g1
	 (ong b2 c2 g2
	   (multiple-value-bind (b3 sign) (,obfn b1 b2 m)
	     (setq g3 (gbc+ g3 b3 (simp `(* ,sign ,c1 ,c2))))))))))

;; Bitmap products (Euclidean & orthogonal)

(defun *gbe (b1 b2)
  "Euclidean geometric product of bitmaps"
  (values (logxor b1 b2)
	  (reordersign b1 b2)))

(defun *ob (b1 b2)
  "Outer product of bitmaps (Euclidean, orthogonal, and non-orthogonal)"
  (if (not (zerop (logand b1 b2)))
      (values 0 0)
      (*gbe b1 b2)))

(defun *gbo (b1 b2 m)
  "Geometric product of bitmaps with orthogonal metric"
  (values (logxor b1 b2)
	  (* (reordersign b1 b2)
	     (pairsign b1 b2 m))))

(defun *ibe (b1 b2)
  "Inner product of bitmaps with Euclidean metric"
  (multiple-value-bind (basis sign) (*gbe b1 b2)
    (if (= (gradeb basis) (- (gradeb b2) (gradeb b1)))
	(values basis sign)
	(values 0 0))))

(defun *ibo (b1 b2 m)
  "Inner product of bitmaps with orthogonal metric"
  (multiple-value-bind (basis sign) (*gbo b1 b2 m)
    (if (= (gradeb basis) (- (gradeb b2) (gradeb b1)))
	(values basis sign)
	(values 0 0))))

(defun *cbe (b1 b2)
  "Commutator product of 2 basis blades in Euclidean space"
  (let ((s1 (* (reordersign b1 b2)))
	(s2 (* (reordersign b2 b1)))
	(basis (logxor b1 b2)))
    (values basis (/ (- s1 s2) 2))))

(defun *cbo (b1 b2 m)
  "Commutator product of 2 basis blades & vector metric in orthogonal space"
  (let ((s1 (* (reordersign b1 b2) (pairsign b1 b2 m)))
	(s2 (* (reordersign b2 b1) (pairsign b2 b1 m)))
	(basis (logxor b1 b2)))
    (values basis (/ (- s1 s2) 2))))

;; Outer product

(defgpe *o2 *ob "Outer product of 2 GA objects. Euclidean, orthogonal, & non-orthogonal metrics.")
(defun *o (&rest args) "Outer product" (reduce #'*o2 args))

;; Non-orthogonal products

(defun transform (class bitmap scale m dim)
  "Transform a basis blade, given bitmap and scale, to a multivector using tranform matrix"
  (let ((al (funcall class 0 scale)))
    (loop for b = bitmap then (ash b -1)
       for i below dim
       until (zerop b)
       do (unless (zerop (logand b 1))
	    (w/g tmp class
	      (loop for j below dim
		 for mji = (aref m j i)
		 do (unless (zerop mji)
		      (ong alb alc al
			(setq tmp (g2+ tmp (*o2 (funcall class alb alc) 
						(funcall class (ash 1 j) mji)))))))
	      (setq al tmp))))
    al))

(defun toeigenbasis (a m)
  "Transform non-orthogonal GA object to from metric to eigen basis"
  (w/newg out a
    (ong b c a
      (setq out (g2+ out (transform (type-of a) b c (inveigmatrix m) (dimension a)))))))

(defun tometricbasis (a m)
  "Transform non-orthogonal GA object back to metric from eigen basis"
  (w/newg out a
    (ong b c a
      (setq out (g2+ out (transform (type-of a) b c (eigmatrix m) (dimension a)))))))

(defmacro defgpno (name ofn doc)
  "Define derived geometric product on non-orthogonal basis given name, corresponding orthogonal function of MVs & vector metric, and documentation string"
  `(defmethod ,name ((g1 g) (g2 g) (m metric))
     ,doc
     (tometricbasis (,ofn (toeigenbasis g1 m) (toeigenbasis g2 m) (eigenmetric m)) m)))

;; Define other geometric products

(defmacro defgp (name efn ofn nofn doc)
  "Define derived geometric product given Euclidean, orthogonal, and non-orthogonal functions."
  `(defmethod ,name ((g1 g) (g2 g))
     ,doc
     (typecase (metric g1)
       (null (,efn g1 g2))
       (vector (,ofn g1 g2 (metric g1)))
       (metric (,nofn g1 g2 (metric g1))))))

(defgpe *g2e *gbe "Geometric product with Euclidean metric")
(defgpo *g2o *gbo "Geometric product with orthogonal metric")
(defgpno *g2no *g2o "Geometric product with non-orthogonal metric")
(defgp *g2 *g2e *g2o *g2no "Geometric product of 2 GA objects")
(defun *g (&rest args) "Geometric product" (reduce #'*g2 args))
(defgpe *i2e *ibe "Inner contraction product with Euclidean basis")
(defgpo *i2o *ibo "Inner contraction product with orthogonal basis")
(defgpno *i2no *i2o "Inner contraction product with non-orthogonal basis")
(defgp *i2 *i2e *i2o *i2no "Inner contraction product")
(defun *i (&rest args) (reduce #'*i2 args))
(defgpe *c2e *cbe "Inner contraction product with Euclidean basis")
(defgpo *c2o *cbo "Inner contraction product with orthogonal basis")
(defgpno *c2no *c2o "Inner contraction product with non-orthogonal basis")
(defgp *c2 *c2e *c2o *c2no "Inner contraction product")
(defun *c (&rest args) (reduce #'*c2 args))

(defmethod scalar ((g g))
  "Scalar part of GA object"
  (gref g 0))

(defmethod *s2 ((g1 g) (g2 g))
  "Scalar product of 2 GA objects"
  (scalar (*g2 g1 g2)))
(defun *s (&rest args) "Scalar product" (reduce #'*s2 args))

;; Reverse

(defmethod revg ((g g))
  "Reverse of GA object"
  (mapg #'(lambda (b c) (simp `(* ,c ,(aref (revtable g) b)))) g))

;; Versor inverse

(defmethod invv ((g g))
  "Inverse of a versor"
  (/gs (revg g) (scalar (*g2 g (revg g)))))

;; Reflection/rotation

(defmethod refl ((g g) (n g))
  "Reflect a GA object by vector (normalized)"
  (*g n g (invv n)))

(defmethod rot ((g g) (r g))
  "Rotate GA object by rotor (normalized)"
  (*g r g (invv r)))

(defmethod spin ((g g) (s g))
  "Spin a GA object by a spinor (not normalized)"
  (*g s g (revg s)))

;; Norms, unit GA objects

(defmethod normr2 ((g g))
  "Reverse norm squared"
  (scalar (*g2 g (revg g))))

(defmethod normr ((g g))
  "Reverse norm"
  (let ((nr2g (normr2 g)))
    (simp `(* (signum ,nr2g) (sqrt (abs ,nr2g))))))

(defmethod norme2 ((g g))
  "Euclidean norm squared"
  (scalar (*g2e g (revg g))))

(defmethod norme ((g g))
  "Euclidean norm"
  (simp `(sqrt ,(norme2 g))))
