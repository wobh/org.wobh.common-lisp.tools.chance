;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.chance-test
  (:nicknames #:chance-test)
  (:local-nicknames (#:chance #:org.wobh.common-lisp.tools.chance))
  (:use #:common-lisp)
  (:export #:test-all)
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.CHANCE-TEST

Basic tests for chance. If this file loads without errors, the
tests passed."))

(in-package #:org.wobh.common-lisp.tools.chance-test)

(defun test-all ()
  (loop
    with tests = '()
    for test in tests
    do (funcall test)
    finally (return t)))
