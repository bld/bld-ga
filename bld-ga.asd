(defpackage :bld.ga.system
  (:use :asdf :cl))
(in-package :bld.ga.system)
(defsystem :bld-ga
  :name "bld-ga"
  :author "Benjamin L. Diedrich <ben@solarsails.info>"
  :version "0.0.1"
  :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
  :license "MIT"
  :description "Geometric algebra with numeric scalar coefficients"
  :depends-on ("bld-gen" "bld-utils" "bld-linalg")
  :components
  ((:file "package")
   (:file "metric" :depends-on ("package"))
   (:file "mv" :depends-on ("metric"))
   (:file "ga" :depends-on ("mv"))))
