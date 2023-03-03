;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.chance
  (:use #:common-lisp)
  (:nicknames #:chance)
  (:export #:random-whole #:random-range
	   #:random-digit #:random-alpha)
  (:export #:random-array-index #:random-array-subscripts)
  (:export #:random-nth #:random-char
	   #:random-svref #:random-elt
	   #:random-aref)
  (:export #:samplef #:make-bias)
  (:export #:nshuffle #:shuffle)
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.CHANCE

Provides chance utilities."))

(in-package #:org.wobh.common-lisp.tools.chance)

;;; Random numbers and characters

(defun random-whole (bound &optional (random-state *random-state*))
  "Return random whole number between 1 and `bound'."
  (declare (type integer bound))
  (1+ (random bound)))

(defun random-range (bound1 bound2 &optional (random-state *random-state*))
  "Return random number between bounds, inclusive."
  (let ((upper (min bound1 bound2))
        (lower (1+ (max bound1 bound2))))
    (+ upper
       (random (- lower upper)))))

(defun random-digit (&optional (base 10) (random-state *random-state*))
  "Return random digit character in given `base', default 10."
  (digit-char (random base)
	      base))

(defun random-alpha (&optional (random-state *random-state*))
  "Return random capital letter character from A-Z."
  (digit-char (+ 10 (random 26)) ; NOFIX: do NOT depend on `random-range'
	      36))


;;; Random array index and subscripts

(defun random-array-index (array &optional (random-state *random-state*))
  "Return random index in array."
  (random (array-total-size array)))

(defun random-array-subscripts (array &optional (random-state *random-state*))
  "Create a list random subscripts in array."
  (mapcar #'random
	  (array-dimensions array)))


;;; Random elements

(defun random-nth (list &optional (random-state *random-state*))
  "Return random item of a list."
  (let ((length-list (length list)))
    (when (< 0 length-list)
      (nth (random length-list)
	   list))))

(defun random-char (string &optional (random-state *random-state*))
  "Return random element of a string."
  (let ((length-string (length string)))
    (when (< 0 length-string)
      (char string
            (random length-string)))))

(defun random-svref (simple-vector &optional (random-state *random-state*))
  "Return random element of a simple-vector."
  (let ((length-simple-vector (length simple-vector)))
    (when (< 0 length-simple-vector)
      (svref simple-vector
             (random length-simple-vector)))))

;; More generic, less efficient
(defun random-elt (sequence &optional (random-state *random-state*))
  "Return random element of any sequence."
  (let ((length-sequence (length sequence)))
    (when (< 0 length-sequence)
      (elt sequence
	   (random length-sequence)))))

(defun random-aref (array &optional (random-state *random-state*))
  "Return random element of an array."
  (row-major-aref array
		  (random-array-index array)))


;;; Samplef and make-bias

(defgeneric samplef (collection &key with random-state)
  (:documentation "Return a procedurally chosen element of collection.")

  (:method ((col list) &key (with #'random-nth) (random-state *random-state*))
    (funcall with col))

  (:method ((col string) &key (with #'random-char) (random-state *random-state*))
    (funcall with col))

  (:method ((col vector) &key (with #'random-svref) (random-state *random-state*))
    (funcall with col))

  (:method ((col array) &key (with #'random-aref) (random-state *random-state*))
    (funcall with col)))

(defun make-bias (weight0 weight1 &rest weights-rest)
  "Return a function that returns a weighted random sample of sequence.

The biased function can be used as the `:with' argument for `samplef'
for any sequence."
  (let ((weights (list* weight0 weight1 weights-rest)))
    (assert (= 1 (reduce #'+ weights))
	    (weights)
	    "Weights ~S should sum to 1" weights)
    (let* ((weights-count (length weights))
           (weighted-norm (reduce #'lcm weights :key #'denominator))
           (weighted-odds (maplist (lambda (weights)
				     (* weighted-norm (reduce #'+ weights)))
                                   weights))
           (weighted-last (first (last weighted-odds))))
      (lambda (sequence &optional (random-state *random-state*))
        (assert (= weights-count (length sequence))
                (weights-count)
                "Sequence ~S must have length ~D" sequence weights-count)
        (let* ((pick (+ weighted-last (random weighted-norm)))
               (index (mod (1- (position-if (lambda (odds) (<= odds pick))
                                            weighted-odds))
                           weights-count)))
          (elt sequence index))))))


;;; Shuffle

(defun nshuffle (sequence &optional (random-state *random-state*))
  "Destructively Knuth shuffle a sequence."
  (let ((size (length sequence)))
    (dotimes (i size sequence)
      (rotatef (elt sequence i)
	       (elt sequence (+ i (random (- size i))))))))

(defun shuffle (sequence &optional (random-state *random-state*))
  "Non-destructively Knuth shuffle a sequence."
  (nshuffle (copy-seq sequence)))
