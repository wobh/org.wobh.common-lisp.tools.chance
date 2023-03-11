;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.chance-test
  (:nicknames #:chance-test)
  (:use #:common-lisp)
  ;; (:use #:chance) ;; maybe?
  (:local-nicknames (#:chance #:org.wobh.common-lisp.tools.chance))
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.CHANCE-TEST

Basic tests for chance. If this file loads without errors, the
tests passed."))

(in-package #:org.wobh.common-lisp.tools.chance-test)

(let* ((rstate (make-random-state t))
       (*random-state* (make-random-state rstate))
       (expect (random 6)))

  (let* ((*random-state* (make-random-state rstate))
         (subject (chance:random-whole 6)))
    (assert (= expect
               (1- subject))))

  (let* ((*random-state* (make-random-state rstate))
         (subject (chance:random-range 3 8)))
    (assert (= expect
               (+ -3 subject))))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar 6)
         (subject (chance:random-digit indyvar)))
    (assert (= expect (digit-char-p subject indyvar)))))

(let* ((rstate (make-random-state t))
       (*random-state* (make-random-state rstate))
       (expect (random 26)))

  (let* ((*random-state* (make-random-state rstate))
         (subject (chance:random-alpha)))
    (assert (= expect
               (+ -10 (digit-char-p subject 36))))))

(let* ((rstate (make-random-state t))
       (*random-state* (make-random-state rstate))
       (expect (random 4)))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar '(:foo :bar :baz :qux))
         (subject (chance:random-nth indyvar)))
    (assert (= expect
               (position subject indyvar))))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar "abcd")
         (subject (chance:random-char indyvar)))
    (assert (= expect
               (position subject indyvar))))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar #(:foo :bar :bax :qux))
         (subject (chance:random-svref indyvar)))
    (assert (= expect
               (position subject indyvar))))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar #(:foo :bar :bax :qux))
         (subject (chance:random-position indyvar)))
    (assert (= expect
               subject))))

;; FIXME: figure out how to test `nshuffle'.
;; (let* ((rstate (make-random-state t))
;;        (*random-state* (make-random-state rstate))
;;        (indyvar #(:foo :bar :baz :qux))
;;        (expect (loop
;;                   for i from 0 below (length indyvar)
;;                   collect (+ i (random i)))))

;;   (let* ((*random-state* (make-random-state rstate))
;;          (subject (nshuffle indyvar)))
;;     (assert ())))
