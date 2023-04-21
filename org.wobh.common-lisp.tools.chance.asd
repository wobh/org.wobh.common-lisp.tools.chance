;; -*- mode: lisp -*-

(defsystem "org.wobh.common-lisp.tools.chance"
  :description "A system of various randomness tools and toys"
  :version "0.0.2"
  :license "Copyright © 2016-2023 William Clifford All rights reserved."
  :author "William Clifford <will@wobh.org>"
  :components ((:file "chance")
               (:file "chance-user"))
  :in-order-to ((test-op (test-op "org.wobh.common-lisp.tools.chance/test"))))

(defsystem "org.wobh.common-lisp.tools.chance/test"
  :description "Tests for the chance system"
  :depends-on ("org.wobh.common-lisp.tools.chance")
  :components ((:file "chance-test"))
  :perform (test-op (o c) (symbol-call :common-lisp
                                       'load
                                       "chance-test")))
