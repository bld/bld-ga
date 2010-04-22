;; Metric implementation

(in-package :bld-ga)

;;(export '(metric make-metric))

(defun transpose (a)
  "Transpose 2D array"
  (destructuring-bind (n m) (array-dimensions a)
    (loop with aout = (make-array (list m n))
       for i below m
       do (loop for j below n
	     do (setf (aref aout i j) (aref a j i)))
       finally (return aout))))

(defclass metric ()
  ((matrix :initarg :matrix :reader matrix)
   (eigenmetric :reader eigenmetric)
   (eigmatrix :reader eigmatrix)
   (inveigmatrix :reader inveigmatrix)
#|   (isdiagonal :reader isdiagonal)
   (iseuclidean :reader iseuclidean)
   (isantieuclidean :reader isantieuclidean)))
   |#))

(defmethod initialize-instance :after ((mt metric) &key)
  (with-slots (matrix eigenmetric eigmatrix inveigmatrix isdiagonal iseuclidean isantieuclidean) mt
    (multiple-value-bind (eigval eigvec) (jacobi matrix)
      (setf eigenmetric eigval)
      (setf eigenmetric (map 'vector #'fround eigenmetric))
      (setf eigmatrix eigvec)
      (setf inveigmatrix (transpose eigvec))
#|    (setf isdiagonal (diagonalp matrix))
    (if (not isdiagonal)
	(setf iseuclidean nil isantieuclidean nil)
	(progn
	  (setf iseuclidean t isantieuclidean t)
	  (loop for i below (.column-count matrix)
	     when (/= (.aref matrix i i) 1d0)
	     do (setf iseuclidean nil)
	     when (/= (.aref matrix i i) -1d0)
	     do (setf isantieuclidean nil))))))
    |#)))

(defun coerce-dfloat-2darray (m)
  "Coerce a 2d array to double float, returning new array"
  (let ((dims (array-dimensions m)))
    (make-array 
     dims
     :element-type 'double-float
     :initial-contents (loop for i below (first dims)
			  collect (loop for j below (second dims)
				     collect (coerce (aref m i j) 'double-float))))))

(defgeneric make-metric (m)
  (:documentation "Make a metric, depending on the argument type."))
(defmethod make-metric ((m null))
  "Null arg returns null - not used in Eucidean spaces"
  nil)
(defmethod make-metric ((m vector))
  "Vector arg returns vector for orthogonal spaces"
  m)
(defmethod make-metric ((m array))
  "2D array arg returns metric class for non-orthogonal spaces"
  (let ((rank (array-rank m))
	(dims (array-dimensions m)))
    (assert (and (eq 2 rank) (apply #'eq dims)))
    (make-instance 
     'metric 
     :matrix (coerce-dfloat-2darray m))))

(defmethod print-object ((a metric) stream)
  (format stream "#<METRIC ~a>" (matrix a)))
