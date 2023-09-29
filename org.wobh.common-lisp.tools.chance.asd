;; -*- mode: lisp -*-

(defsystem "org.wobh.common-lisp.tools.chance"
  :description "A system of various randomness tools and toys"
  :version "0.0.2"
  :license "Copyright © 2016-2023 William Clifford All rights reserved."
  :author "William Clifford <will@wobh.org>"
  :in-order-to ((test-op (test-op "org.wobh.common-lisp.tools.chance/test")))
  :components ((:file "chance")
               (:file "chance-user"
                      :depends-on ("chance"))))

(defsystem "org.wobh.common-lisp.tools.chance/test"
  :description "Tests for the chance system"
  :depends-on ("org.wobh.common-lisp.tools.chance")
  :perform (test-op (o c) (symbol-call 'chance-test
                                       'test-all))
  :components ((:file "chance-test")))
