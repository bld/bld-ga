(in-package :bld-ga)

(defg e2 (e1 e2))

(defg e3 (e1 e2 e3))

(defg h3 (e0 e1 e2 e3))

(defg c2 (no e1 e2 ni) #2a((0 0 0 -1)
			    (0 1 0 0)
			    (0 0 1 0)
			    (-1 0 0 0)))

(defg c3 (no e1 e2 e3 ni) #2a((0 0 0 0 -1)
			       (0 1 0 0 0)
			       (0 0 1 0 0)
			       (0 0 0 1 0)
			       (-1 0 0 0 0)))

