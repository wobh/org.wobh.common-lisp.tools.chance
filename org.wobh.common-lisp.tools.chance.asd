;; -*- mode: lisp -*-

(asdf:defsystem :org.wobh.common-lisp.tools.chance
  :description "A system of various randomness tools and toys"
  :version "0.0.2"
  :license "Copyright © 2016-2023 William Clifford All rights reserved."
  :author "William Clifford <will@wobh.org>"
  :components ((:file "chance")
               (:file "chance-user" :depends-on ("chance"))))

;;; If you want to run the tests after loading the system do:

;;; CL-USER> (load "chance-test")

;;; If it loads without errors the tests passed.

;;; Making a `test-op' is a TODO for another day
