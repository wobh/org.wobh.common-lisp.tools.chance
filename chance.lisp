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
  ;; biased random samplings
  (:export #:sample-biased-alist
           #:random-biased-alist)
  ;; hash-table mappings
  (:export #:sample-hash-table
           #:random-hash-table-mapping)
  (:export #:sample-biased-hash-table
           #:random-biased-hash-table)
  ;; shuffles
  (:export #:nshuffle-string
           #:nshuffle-simple-vector
           #:nshuffle-list
           #:nshuffle-array
           #:shuffle-sequence)
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


;;; Sample biased items

(defun sample-biased-alist (sample-to a-biased-alist
                           &optional (random-state *random-state*))
  "Return vector of weighted random items in `a-biased-alist'.

Where the alist key is the item
      and alist datum is the weight."
  (declare (type (or simple-vector (integer 1)) sample-to)
           (type list a-biased-alist))
  (flet ((this-sampler (sample-size sample)
           (loop
             with to-jump = (random 1.0)
             with to-pass = 1
             for counter from 0
             for (item weight) in a-biased-alist
             for tally = weight then (+ tally weight)
             if (< counter sample-size)
               do (setf (svref sample counter)
                        item)
             else
               do (let* ((to-keep (/ weight tally)))
                    (decf to-jump (* to-keep to-pass))
                    (setf to-pass (* to-pass (- 1 to-keep)))
                    (when (<= to-jump 0)
                      (setf (svref sample (random sample-size random-state))
                            item)
                      (setf to-jump (random 1.0)
                            to-pass 1)))
             finally (return sample))))
    (etypecase sample-to
      ((integer 1) (this-sampler sample-to
                                 (make-array sample-to)))
      (simple-vector (this-sampler (length sample-to)
                                   sample-to)))))


;;; Random biased items

(defun random-biased-alist (a-biased-alist
                            &optional (random-state *random-state*))
  "Return a weighted random item in `a-biased-alist'.

Where the alist key is the item
      and alist datum is the weight."
  (let ((sample (sample-biased-alist 1 a-biased-alist random-state)))
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

(defun sample-biased-hash-table (sample-to a-biased-hash-table
                                 &optional (random-state *random-state*))
  "Return vector of weighted random items in `a-biased-hash-table'.

Where the hash-table key is the item
      and hash-table value is the weight."
  (declare (type (or simple-vector (integer 1)) sample-to)
           (type hash-table a-biased-hash-table))
  (flet ((this-sampler (sample-size sample)
           (loop
             with to-jump = (random 1.0)
             with to-pass = 1
             for counter from 0
             for item being each hash-key of a-biased-hash-table
               using (hash-value weight)
             for tally = weight then (+ tally weight)
             if (< counter sample-size)
               do (setf (svref sample counter)
                        item)
             else
               do (let* ((to-keep (/ weight tally)))
                    (decf to-jump (* to-keep to-pass))
                    (setf to-pass (* to-pass (- 1 to-keep)))
                    (when (<= to-jump 0)
                      (setf (svref sample (random sample-size random-state))
                            item)
                      (setf to-jump (random 1.0)
                            to-pass 1)))
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

(defun random-biased-hash-table (a-biased-hash-table
                                 &optional (random-state *random-state*))
  "Return weighted random item (key) in `a-biased-hash-table'.

Where the hash-table key is the item
      and hash-table value is the weight."
  (let ((sample (sample-biased-hash-table 1 a-biased-hash-table random-state)))
    (svref sample 0)))


;;; Shuffle sequences

(defun nshuffle-string (a-string
                        &optional (random-state *random-state*))
  "Destructively shuffle `a-string'."
  (loop
    with size = (length a-string)
    initially (when (< size 2)
                (return a-string))
    for counter from 2 below size
    for index = (random counter random-state)
    do (rotatef (char a-string index)
                (char a-string counter))
    finally (return a-string)))

(defun nshuffle-simple-vector (a-simple-vector
                               &optional (random-state *random-state*))
  "Destructively shuffle `a-simple-vector'."
  (loop
    with size = (length a-simple-vector)
    initially (when (< size 2)
                (return a-simple-vector))
    for counter from 2 below size
    for index = (random (+ 2 counter) random-state)
    for element across a-simple-vector
    do (rotatef (svref a-simple-vector index)
                (svref a-simple-vector counter))
    finally (return a-simple-vector)))

;; Probably fine.

(defun nshuffle-list (a-list
                      &optional (random-state *random-state*))
  "Destructively shuffle `a-list'."
  (loop
    initially (when (endp (cdr a-list))
                (return a-list))
    for counter from 2
    for index = (random counter random-state)
    for item in a-list
    do (rotatef (nth index a-list)
                (nth counter a-list))
    finally (return a-list)))

(defun nshuffle-array (an-array
                       &optional (random-state *random-state*))
  "Destructively shuffle `a-simple-vector'."
  (loop
    initially (when (< (array-total-size an-array) 2)
                (return an-array))
    for counter from 0 below (array-total-size an-array)
    for index = (random (+ 2 counter) random-state)
    do (rotatef (aref an-array index)
                (aref an-array counter))
    finally (return an-array)))

(defun shuffle-sequence (a-sequence
                         &optional (random-state *random-state*))
  "Return a shuffled copy of `a-sequence'."
  (etypecase a-sequence
    (string (nshuffle-string (copy-seq a-sequence)
                             random-state))
    (simple-vector (nshuffle-simple-vector (copy-seq a-sequence)
                                           random-state))
    (list (nshuffle-list (copy-list a-sequence)
                         random-state))))
