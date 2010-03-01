(defpackage :bld.ga.system
  (:use :asdf :cl))
(in-package :bld.ga.system)
(defsystem :bld-ga
  :name "bld-ga"
  :author "Benjamin L. Diedrich <ben@solarsails.info>"
  :version "0.0.1"
  :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
  :license "MIT"
  :description "Geometric algebra"
  :depends-on ("bld-maxima" "bld-utils")
  :components
  ((:file "package")
   (:file "metric" :depends-on ("package"))
   (:file "mvsym" :depends-on ("metric"))
   (:file "gasym" :depends-on ("mvsym"))))
