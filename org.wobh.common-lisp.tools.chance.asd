;; -*- mode: lisp -*-

(asdf:defsystem :org.wobh.common-lisp.tools.chance
  :description "A system of various randomness tools and toys"
  :version "0.0.1"
  :license "Copyright © 2016-2023 William Clifford All rights reserved."
  :author "William Clifford <will@wobh.org>"
  :components ((:file "chance")
	       (:file "chance-test")
	       (:file "chance-user")))
