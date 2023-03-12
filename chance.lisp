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
  (:export #:sample-string
           #:sample-simple-vector
           #:sample-array)
  ;; list items, associations, properties, and nodes
  (:export #:sample-list
           #:sample-alist
           #:sample-plist
           #:sample-tree)
  (:export #:random-list-item
           #:random-alist-assoc
           #:random-plist-property
           #:random-tree-node)
  ;; hash-table mappings
  (:export #:sample-hash-table
           #:random-hash-table-mapping)
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


;;; Sample random elements in strings, vectors, arrays

(defun sample-string (sample-to a-string
                      &optional (random-state *random-state*))
  "Return vector of random characters in `a-string'."
  (declare (type (or simple-vector (integer 1)) sample-to)
           (type string a-string))
  (flet ((this-sampler (sample-size sample)
           (loop
             with population-size = (length a-string)
             for index below sample-size
             do (setf (svref sample index)
                      (char a-string
                            (random population-size
                                    random-state)))
             finally (return sample))))
    (etypecase sample-to
      ((integer 1) (this-sampler sample-to
                                 (make-array sample-to)))
      (simple-vector (this-sampler (length sample-to)
                                   sample-to)))))

(defun sample-simple-vector (sample-to a-simple-vector
                             &optional (random-state *random-state*))
  "Return vector of random elements in `a-simple-vector'."
  (declare (type (or simple-vector (integer 1)) sample-to)
           (type simple-vector a-simple-vector))
  (flet ((this-sampler (sample-size sample)
           (loop
             with population-size = (length a-simple-vector)
             for index below sample-size
             do (setf (svref sample index)
                      (svref a-simple-vector
                             (random population-size
                                     random-state)))
             finally (return sample))))
    (etypecase sample-to
      ((integer 1) (this-sampler sample-to
                                 (make-array sample-to)))
      (simple-vector (this-sampler (length sample-to)
                                   sample-to)))))

(defun sample-array (sample-to an-array
                     &optional (random-state *random-state*))
  "Return vector of random elements in `an-array'."
    (declare (type (or simple-vector (integer 1)) sample-to)
             (type array an-array))
  (flet ((this-sampler (sample-size sample)
           (loop
             with population-size = (array-total-size an-array)
             for index below sample-size
             do (setf (svref sample index)
                      (row-major-aref an-array
                                      (random population-size
                                              random-state)))
             finally (return sample))))
    (etypecase sample-to
      ((integer 1) (this-sampler sample-to
                                 (make-array sample-to)))
      (simple-vector (this-sampler (length sample-to)
                                   sample-to)))))


;;; Sample list items

(defun sample-list (sample-to a-list
                    &optional (random-state *random-state*))
  "Return vector of random items in `a-list'."
  (declare (type (or simple-vector (integer 1)) sample-to)
           (type list a-list))
  (flet ((this-sampler (sample-size sample)
           (loop
             for counter from 0
             for item in a-list
             if (< counter sample-size)
               do (setf (svref sample counter)
                        item)
             else
               do (let ((index (random counter random-state)))
                    (when (< index sample-size)
                      (setf (svref sample index)
                            item)))
             finally (return sample))))
    (etypecase sample-to
      ((integer 1) (this-sampler sample-to
                                 (make-array sample-to)))
      (simple-vector (this-sampler (length sample-to)
                                   sample-to)))))

(defun sample-alist (sample-to an-alist
                     &optional (random-state *random-state*))
  "Return vector of random associations (key datum pairs) in `an-alist'."
  (sample-list sample-to
               an-alist
               random-state))

(defun sample-plist (sample-to a-plist
                     &optional (random-state *random-state*))
  "Return vector of random properties (indicator value pairs) in `a-plist'. "
  (declare (type (or simple-vector (integer 1)) sample-to)
           (type list a-plist))
  (flet ((this-sampler (sample-size sample)
           (loop
             for counter from 0
             for property in a-plist by #'cddr
             if (< counter sample-size)
               do (setf (svref sample counter)
                        property)
             else
               do (let ((index (random counter random-state)))
                    (when (< index sample-size)
                      (setf (svref sample index)
                            property)))
             finally (return sample))))
    (etypecase sample-to
      ((integer 1) (this-sampler sample-to
                                 (make-array sample-to)))
      (simple-vector (this-sampler (length sample-to)
                                   sample-to)))))

(defun sample-tree (sample-to a-tree
                    &optional (random-state *random-state*))
  "Return vector of random nodes in `a-tree'."
  (declare (type (or simple-vector (integer 1)) sample-to)
           (type list a-tree))
  (flet ((this-sampler (sample-size sample)
           (let ((counter 0))
             (flet ((node-test (node other)
                      (declare (ignore other))
                      (if (< counter sample-size)
                          (setf (svref sample counter)
                                node)
                          (let ((index (random counter random-state)))
                            (when (< index sample-size)
                              (setf (svref sample index)
                                    node))))
                      (incf counter 1)
                      t))
               (tree-equal a-tree a-tree
                           :test #'node-test)
               sample))))
    (etypecase sample-to
      ((integer 1) (this-sampler sample-to
                                 (make-array sample-to)))
      (simple-vector (this-sampler (length sample-to)
                                   sample-to)))))


;;; Random list items

(defun random-list-item (a-list
                         &optional (random-state *random-state*))
  "Return random item in `a-list'."
  (let ((sample (sample-list 1 a-list random-state)))
    (svref sample 0)))

(defun random-alist-assoc (an-alist
                           &optional (random-state *random-state*))
  "Return random association (key-datum pair) in `a-alist'."
  (let ((sample (sample-list 1 an-alist random-state)))
    (svref sample 0)))

(defun random-plist-property (a-plist
                              &optional (random-state *random-state*))
  "Return random property (indicator-value pair) in `a-plist'."
  (let ((sample (sample-plist 1 a-plist random-state)))
    (svref sample 0)))

(defun random-tree-node (a-tree
                         &optional (random-state *random-state*))
  "Return random node in `a-tree'."
  (let ((sample (sample-tree 1 a-tree random-state)))
    (svref sample 0)))


;;; Sample hash-table mappings

(defun sample-hash-table (sample-to a-hash-table
                          &optional (random-state *random-state*))
  "Return vector of random mappings (key-value pairs) in `a-hash-table'."
  (declare (type (or simple-vector (integer 1)) sample-to)
           (type hash-table a-hash-table))
  (flet ((this-sampler (sample-size sample)
           (loop
             for counter from 0
             for a-key being each hash-key of a-hash-table
               using (hash-value a-value)
             if (< counter sample-size)
               do (setf (svref sample counter)
                        (list a-key a-value))
             else
               do (let ((index (random counter random-state)))
                    (when (< index sample-size)
                      (setf (svref sample index)
                            (list a-key a-value))))
             finally (return sample))))
    (etypecase sample-to
      ((integer 1) (this-sampler sample-to
                                 (make-array sample-to)))
      (simple-vector (this-sampler (length sample-to)
                                   sample-to)))))


;;; Random hash-table mapping

(defun random-hash-table-mapping (a-hash-table
                                  &optional (random-state *random-state*))
  "Return random mapping (key-value pair) in `a-hash-table'."
  (let ((sample (sample-hash-table 1 a-hash-table random-state)))
    (svref sample 0)))


;;; Shuffle sequences

(defun nshuffle (a-sequence &optional (random-state *random-state*))
  "Destructively Knuth shuffle `a-sequence'."
  (etypecase a-sequence
    (list (loop
            for counter from 0
            for index = (random (+ 2 counter) random-state)
            for item in a-sequence
            do (rotatef (elt a-sequence index)
                        (elt a-sequence counter))
            finally (return a-sequence)))
    (sequence (loop
                for counter from 0
                for index = (random (+ 2 counter) random-state)
                for element across a-sequence
                do (rotatef (elt a-sequence index)
                            (elt a-sequence counter))
                finally (return a-sequence)))))

(defun shuffle (a-sequence &optional (random-state *random-state*))
  "Return a Knuth shuffled copy of `a-sequence'."
  (nshuffle (copy-seq a-sequence)
            random-state))
