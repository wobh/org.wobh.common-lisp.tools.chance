(defpackage #:org.wobh.common-lisp.tools.randomness-test
  (:use #:cl)
  (:local-nicknames (#:randomness #:org.wobh.common-lisp.tools.randomness))
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.RANDOMNESS-TEST

Basic tests for RANDOMNESS. If this file loads without errors, the
tests passed."))

(in-package #:org.wobh.common-lisp.tools.randomness-test)

(let* ((rstate (make-random-state t))
       (*random-state* (make-random-state rstate))
       (expect (random 6)))

  (let* ((*random-state* (make-random-state rstate))
         (subject (randomness:random-whole 6)))
    (assert (= expect
               (1- subject))))

  (let* ((*random-state* (make-random-state rstate))
         (subject (randomness:random-range 3 8)))
    (assert (= expect
               (+ -3 subject))))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar 6)
         (subject (randomness:random-digit indyvar)))
    (assert (= expect (digit-char-p subject indyvar)))))

(let* ((rstate (make-random-state t))
       (*random-state* (make-random-state rstate))
       (expect (random 26)))

  (let* ((*random-state* (make-random-state rstate))
         (subject (randomness:random-alpha)))
    (assert (= expect
               (+ -10 (digit-char-p subject 36))))))

(let* ((rstate (make-random-state t))
       (*random-state* (make-random-state rstate))
       (expect (random 4)))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar '(:foo :bar :baz :qux))
         (subject (randomness:random-nth indyvar)))
    (assert (= expect
               (position subject indyvar))))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar "abcd")
         (subject (randomness:random-char indyvar)))
    (assert (= expect
               (position subject indyvar))))

  (let* ((*random-state* (make-random-state rstate))
         (indyvar #(:foo :bar :bax :qux))
         (subject (randomness:random-svref indyvar)))
    (assert (= expect
               (position subject indyvar)))))

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
