(asdf:defsystem :bld-ga
  :name "bld-ga"
  :author "Ben Diedrich <bldiedrich@gmail.com>"
  :license "MIT"
  :description "Library implementing geometric algebra. Define algebras based on dimension and inner product signature. Uses BLD-GEN system for generic arithmetic functions. Use BLD-GENSYM for symbolic scalars and coefficients. This implementation is meant to be slow and correct. For speed, see BLD-GAGEN, which adds optimized methods on geometric object types (e.g. vectors, bivectors, spinors) to BLD-GA."
  :depends-on ("bld-gen" "bld-utils" "bld-linalg")
  :components
  ((:file "package")
   (:file "metric" :depends-on ("package"))
   (:file "mv" :depends-on ("metric"))
   (:file "ga" :depends-on ("mv"))
   (:file "linear" :depends-on ("ga"))))

