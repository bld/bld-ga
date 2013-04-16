(in-package :bld-ga)

(defg e2 2 (e1 e2))

(defg e3 3 (e1 e2 e3))

(defg c2 4 (e0 e1 e2 e3) #2a((0 0 0 -1)
			     (0 1 0 0)
			     (0 0 1 0)
			     (-1 0 0 0)))

(defg c3 5 (e0 e1 e2 e3 ei) #2a((0 0 0 0 -1)
				(0 1 0 0 0)
				(0 0 1 0 0)
				(0 0 0 1 0)
				(-1 0 0 0 0)))
