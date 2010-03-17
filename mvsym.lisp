(in-package :bld-ga)

;; Symbolic GA multivector definition

(export '(genrevtable g defg coef metric dimension size revtable bitmap defgfun gref gset w/g loopg collectg ong newg w/newg mapcg mapg cpg w/cpg))

(defun revbsign (b)
  "Sign of reversing a basis bitmap"
  (expt -1 (* 1/2 (logcount b) (- (logcount b) 1))))

(defun genrevtable (dim)
  "Generate reverse sign lookup table"
  (let ((size (expt 2 dim)))
    (make-array 
     size 
     :initial-contents
     (loop for b below size
	collect (revbsign b)))))

(defclass g ()
  ((coef :accessor coef
	 :type simple-vector
	 :initarg :coef)
   (metric :reader metric)
   (dimension :reader dimension)
   (size :reader size)
   (revtable :reader revtable)
   (bitmap :reader bitmap)))

(defmacro defg (name dim &optional metric)
  (let* ((size (expt 2 dim))
	 (bitmap (apply #'vector (loop for b below size collect b))))
    `(defclass ,name (g)
       ((coef :initform (make-array ,size :initial-element 0))
	(metric :allocation :class
		:initform (make-metric ,metric))
	(dimension :allocation :class
		   :initform ,dim)
	(size :allocation :class
	      :initform ,size)
	(revtable :allocation :class
		  :initform (genrevtable ,dim))
	(bitmap :allocation :class
		:initform ,bitmap)))))

(defmacro defgfun (class bitmap)
  "Make a GA object creation function of the given class & bitmap"
  (let* ((args (loop for b across bitmap
		  collect (build-symbol c (:< (format nil "~b" b)))))
	 (args-key (mapcar #'(lambda (arg) (list arg 0)) args)))
    `(defun ,class (&key ,@args-key)
       (make-instance ',class :coef (vector ,@args)))))

;; Macros and functions to provide generic access to GA objects

(defmethod gref ((g g) (bb integer))
  "Reference GA object by basis blade bitmap"
  (aref (coef g) bb))

(defmethod gset ((g g) (bb integer) val)
  "Set GA object of given basis blade bitmap to value"
  (setf (aref (coef g) bb) val))

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
  `(loop for ,b across (bitmap ,g)
      for ,c across (coef ,g)
	,@body))

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
  (make-instance (type-of (first gs))
		 :coef (apply #'map 'vector f (mapcar #'coef gs))))

(defmethod mapg (f (g g))
  "Map a function of bitmaps and coefficients across one GA object, returning new one with function results as coefficients"
  (make-instance (type-of g)
		 :coef (map 'vector f (bitmap g) (coef g))))

(defmethod cpg ((g g))
  "Copy GA object"
  (make-instance (type-of g) :coef (copy-seq (coef g))))

(defmacro w/cpg (gc g &body body)
  "Copy G into GC, evaluate body, and return GC"
  `(let ((,gc (cpg ,g)))
     ,@body
     ,gc))

;; Print GA object

(defmethod print-object ((g g) stream)
  (format stream "#<~a" (type-of g))
  (ong b c g
    (format stream " #b~b ~a" b c))
  (format stream ">"))
