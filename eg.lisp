(in-package :bld-ga)

(defg e2 2)
(defgfun e2 2)

(defg e3 3)
(defgfun e3 3)

(defg c2 4 #2a((0 0 0 -1)
	       (0 1 0 0)
	       (0 0 1 0)
	       (-1 0 0 0)))
(defgfun c2 4)

(defg c3 5 #2a((0 0 0 0 -1)
	       (0 1 0 0 0)
	       (0 0 1 0 0)
	       (0 0 0 1 0)
	       (-1 0 0 0 0)))
(defgfun c3 5)
