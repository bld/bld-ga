;; Examples of usage of BLD-GA
;; Used for top-down development of the library

(in-package :bld-ga)

(defga e2ga 2
    :basis (e1 e2) 
    :metric ((e1 e1 1) (e2 e2 1)))

(defga c2ga 4
    :basis (no e1 e2 ni)
    :metric ((e1 e1 1)
	     (e2 e2 1)
	     (no ni -1)))

