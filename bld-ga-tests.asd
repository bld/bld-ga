(asdf:defsystem :bld-ga-tests
  :name "bld-ga-tests"
  :author "Ben Diedrich <bldiedrich@gmail.com>"
  :license "MIT"
  :description "Unit tests of BLD-GA geometric algebra library"
  :depends-on ("bld-ga" "fiveam")
  :components ((:file "tests")))
