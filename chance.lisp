;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.chance
  (:nicknames #:chance)
  (:use #:common-lisp)
  ;; numbers and characters
  (:export #:random-whole
           #:random-range
           #:random-digit
           #:random-alpha)
  ;; sequence position and array index, subscripts
  (:export #:random-position
           #:random-array-row-major-index
           #:random-array-subscripts)
  ;; sequence elements
  (:export #:random-char
           #:random-svref
           #:random-aref
           #:random-nth
           #:random-elt)
  ;; shuffles
  (:export #:nshuffle
           #:shuffle)
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.CHANCE

Provides chance utilities."))

(in-package #:org.wobh.common-lisp.tools.chance)

;;; Random numbers and characters

(defun random-whole (bound &optional (random-state *random-state*))
  "Return random whole number between 1 and `bound'."
  (declare (type integer bound))
  (1+ (random bound
              random-state)))

(defun random-range (bound1 bound2 &optional (random-state *random-state*))
  "Return random number between bounds, inclusive."
  (let ((upper (min bound1 bound2))
        (lower (1+ (max bound1 bound2))))
    (+ upper
       (random (- lower upper)
               random-state))))

(defun random-digit (&optional (base 10) (random-state *random-state*))
  "Return random digit character in given `base', default 10."
  (digit-char (random base random-state)
              base))

(defun random-alpha (&optional (random-state *random-state*))
  "Return random capital letter character from A-Z."
  (digit-char (+ 10 (random 26 random-state)) ; NOFIX: do NOT depend on `random-range'
              36))


;;; Random sequence position, array index or subscripts

(defun random-position (a-sequence &optional (random-state *random-state*))
  "Return random position (index) in `a-sequence'."
  (let ((limit (length a-sequence)))
    (random limit
            random-state)))

(defun random-array-row-major-index (an-array &optional (random-state *random-state*))
  "Return random index in `an-array'."
  (let ((limit (array-total-size an-array)))
    (random limit
            random-state)))

(defun random-array-subscripts (an-array &optional (random-state *random-state*))
  "Return a list random subscripts in `an-array'."
  (flet ((this-random (limit)
           (random limit random-state)))
    (mapcar #'this-random
            (array-dimensions an-array))))


;;; Random elements of sequences or arrays

(defun random-char (a-string &optional (random-state *random-state*))
  "Return random character in `a-string'."
  (let ((limit (length a-string)))
    (char a-string
          (random limit random-state))))

(defun random-svref (a-simple-vector &optional (random-state *random-state*))
  "Return random element in `a-simple-vector'."
  (let ((limit (length a-simple-vector)))
    (svref a-simple-vector
           (random limit random-state))))

(defun random-aref (an-array &optional (random-state *random-state*))
  "Return random element in `an-array'."
  (let ((limit (array-total-size an-array)))
    (row-major-aref an-array
                    (random limit random-state))))

;; less efficient, but probably fine

(defun random-nth (a-list &optional (random-state *random-state*))
  "Return random item in `a-list'."
  (let ((limit (list-length a-list)))
    (nth (random limit random-state)
         a-list)))

(defun random-elt (a-sequence &optional (random-state *random-state*))
  "Return random element in `a-sequence' of any type."
  (let ((limit (length a-sequence)))
    (elt a-sequence
         (random limit random-state))))


;;; Shuffle sequences

(defun nshuffle (a-sequence &optional (random-state *random-state*))
  "Destructively Knuth shuffle `a-sequence'."
  (let ((seqlen (length a-sequence)))
    (dotimes (index seqlen a-sequence)
      (rotatef (elt a-sequence index)
               (elt a-sequence
                    (+ index
                       (random (- seqlen index)
                               random-state)))))))

(defun shuffle (a-sequence &optional (random-state *random-state*))
  "Return a Knuth shuffled copy of `a-sequence'."
  (nshuffle (copy-seq a-sequence)
            random-state))
