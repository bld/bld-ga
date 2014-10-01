(in-package :bld-ga)

;; Symbolic GA multivector definition

(defun revbsign (b)
  "Sign of reversing a basis bitmap"
  (expt -1 (* 1/2 (logcount b) (- (logcount b) 1))))

(defun make-bitmap (size)
  "Make hash table mapping basis blade to binary representation"
  (make-array size
	      :initial-contents
	      (loop for b below size
		 collect b)))

(defun make-revtable (size)
  "Make hash table mapping reverse to basis blade"
  (make-array size
	      :initial-contents
	      (loop for b below size
		 collect (revbsign b))))

(defclass g ()
  ((metric :reader metric)
   (dimension :reader dimension)
   (size :reader size)
   (revtable :reader revtable)
   (bitmap :reader bitmap)
   (unitvectors :reader unitvectors)
   (basisblades :reader basisblades)
   (basisbladekeys :reader basisbladekeys)))

(defun make-basis-blades (unitvectors)
  "Make a vector of basis blades as a list of unit vector from a vector of unit vectors. Position in the array - in binary - is the binary basis blade representation. Append unit vector symbols. S for scalar."
  (let* ((dim (length unitvectors))
	 (size (expt 2 dim)))
    (loop for b below size
       collect (loop for i below dim
		  unless (zerop (logand (expt 2 i) b))
		  collect (nth i unitvectors) into bb
		  finally (return (if bb
				      (intern (apply #'concatenate 'string (mapcar #'string bb)))
				      (intern "S")))))))

(defmacro defgfun (class basisblades basisbladekeys)
  "Make a GA object creation function of the given class & bitmap"
  `(defun ,class (&key ,@(loop for bb in basisblades
			    collect `(,bb 0)))
     (make-instance 
      ',class
      ,@(loop for bb in basisblades
	   for bbk in basisbladekeys
	   collect bbk
	   collect bb))))

(defmacro defg (name unitvectors &optional metric)
  "Define a geometric algebra given the name, dimension, (unit
vectors), and optional inner product metric (vector or 2D array)."
  (let* ((dim (length unitvectors))
	 (size (expt 2 dim))
	 (basisblades (make-basis-blades unitvectors))
	 (basisbladekeys (mapcar #'make-keyword basisblades)))
    `(progn
       (defclass ,name (g)
	 ((metric :allocation :class
		  :initform (make-metric ,metric))
	  (dimension :allocation :class
		     :initform ,dim)
	  (size :allocation :class
		:initform ,size)
	  (bitmap :allocation :class
		  :initform (make-bitmap ,size))
	  (revtable :allocation :class
		    :initform (make-revtable ,size))
	  (unitvectors :allocation :class
		       :initform ',unitvectors)
	  (basisblades :allocation :class
		       :initform ',basisblades)
	  (basisbladekeys :allocation :class
			  :initform ',basisbladekeys)
	  ,@(loop for bb in basisblades
	       for bbk in basisbladekeys
	       collect `(,bb :initarg ,bbk :initform 0))))
       (defgfun ,name ,basisblades ,basisbladekeys))))

;;; Macros and functions to provide generic access to GA objects

(defmethod gref ((g g) (bb symbol))
  "Reference GA object by basis blade keyword name"
  (slot-value g (find-symbol (symbol-name bb) 'bld-ga)))

(defmethod gset ((g g) (bb symbol) val)
  "Set GA object of given basis blade keyword name to value"
  (setf (slot-value g (find-symbol (symbol-name bb) 'bld-ga)) val))

(defmethod gref ((g g) (bb integer))
  "Reference GA object by basis blade bitmap"
  (slot-value g (elt (basisblades g) bb)))

(defmethod gset ((g g) (bb integer) val)
  "Set GA object of given basis blade bitmap to value"
  (setf (slot-value g (elt (basisblades g) bb)) val))

(defsetf gref gset)

(defun numberzerop (n)
  "Return true if n is the number zero. NIL if non-zero or a non-number (e.g. symbol)."
  (and (numberp n) (zerop n)))

(defmacro w/g (name class &body body)
  "Inspired by Arc's w/ forms. Instantiate GA object of given name & class, and return it after BODY forms are evaluated."
  `(let ((,name (make-instance ,class)))
     ,@body
     ,name))

(defmacro loopg (b c g &body body)
  "Loop across the basis-bitmaps and coefficients of a GA object, evaluating the given LOOP forms"
  (let ((bb (gensym)))
    `(loop for ,b across (bitmap ,g)
	for ,bb in (basisblades ,g)
	for ,c = (slot-value ,g ,bb)
	  ,@body)))

(defmacro collectg (b c g form)
  "Loop over the basis-bitmaps and coefficients of a GA object, collecting the results of evaluating FORM into a list"
  `(loopg ,b ,c ,g collect ,form))

(defmacro ong (b c g &body body)
  "Loop over a GA object's basis bitmaps and coefficients. Execute BODY when coefficient is non-zero."
  `(loopg ,b ,c ,g
      unless (or (null ,c) (numberzerop ,c))
      do ,@body))

(defmethod newg ((g g))
  "Create a new GA object of the same type given"
  (make-instance (type-of g)))

(defmacro w/newg (new template &body body)
  "Create new GA object of the same type as TEMPLATE, evaluate BODY forms, and return NEW"
  `(let ((,new (newg ,template)))
     ,@body
     ,new))

(defun mapcg (f &rest gs)
  "Return new GA object with coefficients equal to a function mapped across the coefficients the given GA objects"
  (w/newg gout (first gs)
    (dolist (bb (basisblades gout))
      (setf (slot-value gout bb)
	    (apply f (mapcar #'(lambda (g) (slot-value g bb)) gs))))))

(defmethod mapg (f (g g))
  "Map a function of bitmaps and coefficients across one GA object, returning new one with function results as coefficients"
  (w/newg gout g
    (loop for bb in (basisblades g)
       for b across (bitmap g)
       do (setf (slot-value gout bb)
		(funcall f b (slot-value g bb))))))

(defmethod cpg ((g g))
  "Copy GA object"
  (w/newg gout g
    (loop for bb in (basisblades g)
       do (setf (slot-value gout bb) (slot-value g bb)))))

(defmacro w/cpg (gc g &body body)
  "Copy G into GC, evaluate body, and return GC"
  `(let ((,gc (cpg ,g)))
     ,@body
     ,gc))

(defun makeg (class &rest args)
  "Create GA object of specified class. Provide basis-bitmaps & coefficients to populate.
E.g. (makeg 'e2 :s 1 :e1e2 2)"
  (apply #'make-instance class args))

;; Print GA object

(defmethod print-object ((g g) stream)
  (format stream "#<~a~{ :~a ~a~}>"
	  (type-of g)
	  (loop for bb in (basisblades g)
	     for c = (slot-value g bb)
	     unless (numberzerop c)
	     collect bb and collect c)))
