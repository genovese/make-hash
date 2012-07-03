;;;; tests -- test suites for make-hash pacakge
;;;
;;; Copyright (C) 2012 Christopher R. Genovese, all rights reserved.
;;;
;;; Author: Christopher Genovese <genovese@cmu.edu>
;;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;;; URL: http://github.com/genovese/make-hash
;;;
;;; Version: 1.0.0
;;; Update#: 5
;;; Created:      Wed 18 Apr 2012 at 09:55 EDT
;;; Last-Updated: Tue 03 Jul 2012 at 13:00:02 EDT
;;; Updated By: Christopher R. Genovese


(defpackage #:make-hash-tests
  (:use #:cl #:make-hash #:fiveam))

(in-package :make-hash-tests)


;;; Administrative interface for recording and running tests

(defvar *my-test-suites* nil)

(defmacro defsuite (name &rest body+)
  "Define a test suite NAME and add it to the list of defined suites. 
BODY+ can begin with an optional descriptive string followed by zero or
more forms that are executed with the suite NAME active. This would
typically be used to define tests within a suite, although tests can
also be added to the suite later using `in-suite'."
  (let* ((description (if (stringp (first body+))
                          (list :description (first body+))
                          nil))
         (body (if description (rest body+) body+)))
    `(progn
       (def-suite ,name
           ,@description)
       (in-suite ,name)
       ,@body
       (in-suite nil)
       (pushnew ',name *my-test-suites*))))

(defun run-tests (&rest args)
  "Run all defined test suites with explanations."
  (declare (ignorable args))
  (loop for suite in *my-test-suites* do
        (run! suite)))


;;; Test Data 
;;  Note that all hashes here store integers as values for easy testing.

(defparameter *empty-hash* (make-hash-table))

(defparameter *alpha-hash* (make-hash-table :test #'eql))
(loop for c from 65 to 90 do
      (let ((ch (code-char c)))
        (setf (gethash ch *alpha-hash*) (+ 1 (random 10)))))


(defparameter *symbol-hash* (make-hash-table :test #'eq))
(defparameter *symbol-alist* nil)
(loop for c from 65 to 90 do
      (let ((sym (intern (string (code-char c)))))
        (setf (gethash sym *symbol-hash*) c)
        (push (cons sym c) *symbol-alist*)))
(loop for feature in *features* do
      (let ((entry (random 1024)))
        (setf (gethash feature *symbol-hash*) entry )
        (push (cons feature entry) *symbol-alist*)))

(defparameter *keyword-hash* (make-hash-table :test #'eq))
(defparameter *keyword-alist* nil)
(loop for c from 65 to 90 do
      (let ((sym (intern (string (code-char c)) :keyword))
            (val (* (- c 65) 10)))
        (setf (gethash sym *keyword-hash*) val)
        (push (cons sym val) *keyword-alist*)))
(loop for feature in (remove-if-not #'keywordp *features*) do
      (let ((entry (* 1000 (+ (random 1024) 1))))
        (setf (gethash feature *keyword-hash*) entry )
        (push (cons feature entry) *keyword-alist*)))

(defparameter *string-hash* (make-hash-table :test #'equal))
(defparameter *string-alist* nil)
(loop for feature in (mapcar (lambda (s) (string-downcase (symbol-name s)))
                             *features*)
      do
      (let ((entry (random 1024)))
        (setf (gethash feature *string-hash*) entry)
        (push (cons feature entry) *string-alist*)))

(defparameter *string-nc-hash* (make-hash-table :test #'equalp))
(defparameter *string-nc-alist* nil)
(loop for feature in (mapcar (lambda (s) (string-downcase (symbol-name s)))
                             *features*)
      do
      (let ((entry (random 1024)))
        (setf (gethash feature *string-nc-hash*) entry)
        (push (cons feature entry) *string-nc-alist*)))

(defparameter *random-int-bag*
  (loop for i from 0 below 1024 collect (random 128)))
(defparameter *bag-truth* (make-hash-table :size 128 :test #'eql))
(loop for k in *random-int-bag* do (incf (gethash k *bag-truth* 0)))


;;; Utility and Data-Creation Functions 

(defun make-random-int-hash (n type)
  (declare (type (integer 0 *) n)
           (type (or (eql :hash) (eql :alist)) type))
  (ecase type
    (:hash
     (loop with h = (make-hash-table :size n :test #'eql)
           for key from 0 below n 
           for val = (random n)
           do (setf (gethash key h) val)
           finally (return h)))
    (:alist
     (loop with h = nil
           for key from 0 below n 
           for val = (random n)
           do (push (cons key val) h)
           finally (return h)))))

(defun as-vector (x)
  (coerce x 'vector))

(defun hash-to-alist (hash)
  (loop for key being each hash-key of hash using (hash-value val)
        collect (cons key val)))

(defun hash-to-keys (hash)
  (loop for key being each hash-key of hash
        collect key))

(defun pair-to-list (pair)
  (list (car pair) (cdr pair)))

(defun pair-to-vector (pair)
  (vector (car pair) (cdr pair)))

(defun flatten-alist (alist)
  (mapcan #'pair-to-list alist))

(defun lisify-alist (alist)
  (mapcar #'pair-to-list alist))

(defun vecify-alist (alist)
  (mapcar #'pair-to-vector alist))

(defun seqify-alist (alist)
  (mapcar (lambda (pair)
            (if (= 1 (random 2))
                (pair-to-list pair)
                (pair-to-vector pair)))
          alist))

(defun random-integer (a b &optional (n 1))
  "Generate N uniform random integers A <= x < B.
If N == 1, return a number otherwise a list of numbers."
  (let ((sample
         (loop for i from 0 below n collect
               (+ a (random (- b a))))))
    (if (= n 1) (first sample) sample)))

(defun draw-iid-sample (outcomes &optional (n 1))
  "Generate an iid sample of size N from the set OUTCOMES.
Return a list of values or a single value if N == 1. OUTCOMES must be
coercable to a vector."
  (let* ((ovec (coerce outcomes 'vector))
         (olen (length ovec))
         (sample
          (loop for i from 0 below n collect
                (aref ovec (random-integer 0 olen)))))
    (if (= n 1) (first sample) sample)))


;;; Comparisons of new hash with reference

(defgeneric compare-hash (candidate reference))

;; We do not use equalp because we are ignoring the test comparison
;;(defmethod compare-hash ((candidate hash-table) (reference hash-table))
;;  (is (equalp candidate reference)))

(defmethod compare-hash ((candidate hash-table) (reference hash-table))
  (is (= (hash-table-count candidate) (hash-table-count reference)))
  (loop for key being each hash-key of reference using (hash-value val)
        do
        (is (equal (gethash key candidate) val))))

(defmethod compare-hash ((candidate hash-table) (reference list))
  (is (= (hash-table-count candidate) (list-length reference)))
  (loop for (key . val) in reference
        do
        (is (equal (gethash key candidate) val))))


;;; Test Suites

(defsuite hash-initializer
  "Create hash tables from an existing hash table."
  (test hash-whole-0
    "Initialize from an empy existing hash table."
    (let ((table (make-hash :initial-contents *empty-hash* :init-format :hash)))
      (is (zerop (hash-table-count table)))))

  (test hash-whole-1
    "Initialize from an entire existing hash table."
    (let* ((table0 *keyword-hash*)
           (table1 (make-hash :test (hash-table-test table0)
                              :initial-contents table0
                              :init-format :hash)))
      (compare-hash table1 table0)))

  (test hash-whole-2
    "Initialize from an entire existing hash table."
    (let* ((table0 *symbol-hash*)
           (table1 (make-hash :test (hash-table-test table0)
                              :initial-contents table0
                              :init-format :hash)))
      (compare-hash table1 table0)))

  (test hash-whole-3
    "Initialize from an entire existing hash table."
    (let* ((table0 *string-hash*)
           (table1 (make-hash :test (hash-table-test table0)
                              :initial-contents table0
                              :init-format :hash)))
      (compare-hash table1 table0)))

  (test hash-whole-4
    "Initialize from an entire existing hash table."
    (let* ((table0 *string-nc-hash*)
           (table1 (make-hash :test (hash-table-test table0)
                              :initial-contents table0
                              :init-format :hash)))
      (compare-hash table1 table0)))

  (test hash-whole-5
    "Initialize from an entire existing hash table."
    (dotimes (_ 5)
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table1 (make-hash :test (hash-table-test table0)
                                :initial-contents table0
                                :init-format :hash)))
        (compare-hash table1 table0))))

  (test hash-whole-6
    "Initialize from an entire existing hash table. No init-format supplied."
    (let* ((table0 *keyword-hash*)
           (table1 (make-hash :test (hash-table-test table0)
                              :initial-contents table0)))
      (compare-hash table1 table0))))


(defsuite flat-initializer
    "Create hash tables with sequence of alternating keys and values"
  (test flat-0
    "Initialize from an empy flat list and vector."
    (let ((tableL (make-hash :initial-contents '() :init-format :flat))
          (tableV (make-hash :initial-contents #() :init-format :flat)))
      (is (zerop (hash-table-count tableL)))
      (is (zerop (hash-table-count tableV)))))

  (test flat-1
    "Initialize from a flat list."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (flatten-alist table0)
                              :init-format :flat)))
      (compare-hash table1 table0)))
  
  (test flat-2
    "Initialize from a flat list."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (flatten-alist table0)
                              :init-format :flat)))
      (compare-hash table1 table0)))

  (test flat-3
    "Initialize from a flat list."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (flatten-alist table0)
                              :init-format :flat)))
      (compare-hash table1 table0)))

  (test flat-4
    "Initialize from a flat list."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (flatten-alist table0)
                              :init-format :flat)))
      (compare-hash table1 table0)))
  
  (test flat-5
    "Initialize from a flat vectors."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (flatten-alist table0))
                              :init-format :flat)))
      (compare-hash table1 table0)))
  
  (test flat-6
    "Initialize from a flat vector."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (flatten-alist table0))
                              :init-format :flat)))
      (compare-hash table1 table0)))

  (test flat-7
    "Initialize from a flat vector."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (as-vector (flatten-alist table0))
                              :init-format :flat)))
      (compare-hash table1 table0)))

  (test flat-8
    "Initialize from a flat vector."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (as-vector (flatten-alist table0))
                              :init-format :flat)))
      (compare-hash table1 table0)))

  (test flat-9
    "Initialize from a transformed flat list."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (flatten-alist (hash-to-alist table0))
                                :init-format :flat
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans))))

  (test flat-10
    "Initialize from a transformed flat vector."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (as-vector (flatten-alist (hash-to-alist table0)))
                                :init-format :flat
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans)))))

(defsuite pairs-initializer
    "Create hash tables with sequence of cons pairs."
  (test pairs-0
    "Initialize from an empy flat list and vector."
    (let ((tableL (make-hash :initial-contents '() :init-format :pairs))
          (tableV (make-hash :initial-contents #() :init-format :pairs)))
      (is (zerop (hash-table-count tableL)))
      (is (zerop (hash-table-count tableV)))))

  (test pairs-1
    "Initialize from a flat list."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents table0
                              :init-format :pairs)))
      (compare-hash table1 table0)))
  
  (test pairs-2
    "Initialize from a flat list."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents table0
                              :init-format :pairs)))
      (compare-hash table1 table0)))

  (test pairs-3
    "Initialize from a flat list."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents table0
                              :init-format :pairs)))
      (compare-hash table1 table0)))

  (test pairs-4
    "Initialize from a flat list."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents table0
                              :init-format :pairs)))
      (compare-hash table1 table0)))
  
  (test pairs-5
    "Initialize from a flat vectors."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector table0)
                              :init-format :pairs)))
      (compare-hash table1 table0)))
  
  (test pairs-6
    "Initialize from a flat vector."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector table0)
                              :init-format :pairs)))
      (compare-hash table1 table0)))

  (test pairs-7
    "Initialize from a flat vector."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (as-vector table0)
                              :init-format :pairs)))
      (compare-hash table1 table0)))

  (test pairs-8
    "Initialize from a flat vector."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (as-vector table0)
                              :init-format :pairs)))
      (compare-hash table1 table0)))

  (test pairs-9
    "Initialize from a transformed flat list."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (hash-to-alist table0)
                                :init-format :pairs
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans))))

  (test pairs-10
    "Initialize from a transformed flat vector."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (as-vector (hash-to-alist table0))
                                :init-format :pairs
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans)))))

(defsuite lists-initializer
    "Create hash tables with sequence of pairs as lists."
  (test lists-0
    "Initialize from an empy lists list and vector."
    (let ((tableL (make-hash :initial-contents '() :init-format :lists))
          (tableV (make-hash :initial-contents #() :init-format :lists)))
      (is (zerop (hash-table-count tableL)))
      (is (zerop (hash-table-count tableV)))))

  (test lists-1
    "Initialize from a lists list."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (lisify-alist table0)
                              :init-format :lists)))
      (compare-hash table1 table0)))
  
  (test lists-2
    "Initialize from a lists list."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (lisify-alist table0)
                              :init-format :lists)))
      (compare-hash table1 table0)))

  (test lists-3
    "Initialize from a lists list."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (lisify-alist table0)
                              :init-format :lists)))
      (compare-hash table1 table0)))

  (test lists-4
    "Initialize from a lists list."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (lisify-alist table0)
                              :init-format :lists)))
      (compare-hash table1 table0)))
  
  (test lists-5
    "Initialize from a lists vectors."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (lisify-alist table0))
                              :init-format :lists)))
      (compare-hash table1 table0)))
  
  (test lists-6
    "Initialize from a lists vector."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (lisify-alist table0))
                              :init-format :lists)))
      (compare-hash table1 table0)))

  (test lists-7
    "Initialize from a lists vector."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (as-vector (lisify-alist table0))
                              :init-format :lists)))
      (compare-hash table1 table0)))

  (test lists-8
    "Initialize from a lists vector."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (as-vector (lisify-alist table0))
                              :init-format :lists)))
      (compare-hash table1 table0)))

  (test lists-9
    "Initialize from a transformed lists list."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (lisify-alist (hash-to-alist table0))
                                :init-format :lists
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans))))

  (test lists-10
    "Initialize from a transformed lists vector."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (as-vector (lisify-alist (hash-to-alist table0)))
                                :init-format :lists
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans)))))

(defsuite vectors-initializer
    "Create hash tables with sequence of pairs as vectors."
  (test vectors-0
    "Initialize from an empy vectors list and vector."
    (let ((tableL (make-hash :initial-contents '() :init-format :vectors))
          (tableV (make-hash :initial-contents #() :init-format :vectors)))
      (is (zerop (hash-table-count tableL)))
      (is (zerop (hash-table-count tableV)))))

  (test vectors-1
    "Initialize from a vectors list."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (vecify-alist table0)
                              :init-format :vectors)))
      (compare-hash table1 table0)))
  
  (test vectors-2
    "Initialize from a vectors list."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (vecify-alist table0)
                              :init-format :vectors)))
      (compare-hash table1 table0)))

  (test vectors-3
    "Initialize from a vectors list."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (vecify-alist table0)
                              :init-format :vectors)))
      (compare-hash table1 table0)))

  (test vectors-4
    "Initialize from a vectors list."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (vecify-alist table0)
                              :init-format :vectors)))
      (compare-hash table1 table0)))
  
  (test vectors-5
    "Initialize from a vectors vectors."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (vecify-alist table0))
                              :init-format :vectors)))
      (compare-hash table1 table0)))
  
  (test vectors-6
    "Initialize from a vectors vector."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (vecify-alist table0))
                              :init-format :vectors)))
      (compare-hash table1 table0)))

  (test vectors-7
    "Initialize from a vectors vector."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (as-vector (vecify-alist table0))
                              :init-format :vectors)))
      (compare-hash table1 table0)))

  (test vectors-8
    "Initialize from a vectors vector."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (as-vector (vecify-alist table0))
                              :init-format :vectors)))
      (compare-hash table1 table0)))

  (test vectors-9
    "Initialize from a transformed vectors list."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (vecify-alist (hash-to-alist table0))
                                :init-format :vectors
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans))))

  (test vectors-10
    "Initialize from a transformed vectors vector."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (as-vector (vecify-alist (hash-to-alist table0)))
                                :init-format :vectors
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans)))))

(defsuite seqs-initializer
    "Create hash tables with sequence of pairs that can be lists or vectors."
  (test seqs-0
    "Initialize from an empy seqs list and vector."
    (let ((tableL (make-hash :initial-contents '() :init-format :seqs))
          (tableV (make-hash :initial-contents #() :init-format :seqs)))
      (is (zerop (hash-table-count tableL)))
      (is (zerop (hash-table-count tableV)))))

  (test seqs-1
    "Initialize from a seqs list."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (seqify-alist table0)
                              :init-format :seqs)))
      (compare-hash table1 table0)))
  
  (test seqs-2
    "Initialize from a seqs list."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (seqify-alist table0)
                              :init-format :seqs)))
      (compare-hash table1 table0)))

  (test seqs-3
    "Initialize from a seqs list."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (seqify-alist table0)
                              :init-format :seqs)))
      (compare-hash table1 table0)))

  (test seqs-4
    "Initialize from a seqs list."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (seqify-alist table0)
                              :init-format :seqs)))
      (compare-hash table1 table0)))
  
  (test seqs-5
    "Initialize from a seqs vectors."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (seqify-alist table0))
                              :init-format :seqs)))
      (compare-hash table1 table0)))
  
  (test seqs-6
    "Initialize from a seqs vector."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (seqify-alist table0))
                              :init-format :seqs)))
      (compare-hash table1 table0)))

  (test seqs-7
    "Initialize from a seqs vector."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (as-vector (seqify-alist table0))
                              :init-format :seqs)))
      (compare-hash table1 table0)))

  (test seqs-8
    "Initialize from a seqs vector."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (as-vector (seqify-alist table0))
                              :init-format :seqs)))
      (compare-hash table1 table0)))

  (test seqs-9
    "Initialize from a transformed seqs list."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (seqify-alist (hash-to-alist table0))
                                :init-format :seqs
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans))))

  (test seqs-10
    "Initialize from a transformed seqs vector."
    (flet ((transform (n) (+ n 10000)))
      (let* ((table0 (make-random-int-hash 1024 :hash))
             (table0trans (make-hash-table :test (hash-table-test table0)
                                           :size (hash-table-count table0)))
             (table1 (make-hash :test #'eql
                                :initial-contents (as-vector (seqify-alist (hash-to-alist table0)))
                                :init-format :seqs
                                :init-data (lambda (k v) (values k (transform v) nil)))))
        (maphash (lambda (k v) (setf (gethash k table0trans) (transform v))) table0)
        (compare-hash table1 table0trans)))))

(defsuite keys-initializer
    "Create hash tables with key sequence and data to resolve keys to values."
  (test keys-0
    "Initialize from an empy key list and vector."
    (let ((tableL (make-hash :initial-contents '() :init-format :keys))
          (tableV (make-hash :initial-contents #() :init-format :keys)))
      (is (zerop (hash-table-count tableL)))
      (is (zerop (hash-table-count tableV)))))

  (test keys-1
    "Initialize from a keys list and a hash table."
    (let* ((table0 *keyword-hash*)
           (table1 (make-hash :test #'eq
                              :initial-contents (hash-to-keys table0)
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))
  
  (test keys-2
    "Initialize from a keys list and a hash table."
    (let* ((table0 *symbols-hash*)
           (table1 (make-hash :test #'eq
                              :initial-contents (hash-to-keys table0)
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-3
    "Initialize from a keys list and a hash table."
    (let* ((table0 *string-hash*)
           (table1 (make-hash :test #'equal
                              :initial-contents (hash-to-keys table0)
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-4
    "Initialize from a keys list and a hash table."
    (let* ((table0 (make-random-int-hash 1024 :hash))
           (table1 (make-hash :test #'eql
                              :initial-contents (hash-to-keys table0)
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-5
    "Initialize from a keys vector and a hash table."
    (let* ((table0 *keyword-hash*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (hash-to-keys table0))
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))
  
  (test keys-6
    "Initialize from a keys vector and a hash table."
    (let* ((table0 *symbols-hash*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (hash-to-keys table0))
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-7
    "Initialize from a keys vector and a hash table."
    (let* ((table0 *string-hash*)
           (table1 (make-hash :test #'equal
                              :initial-contents (as-vector (hash-to-keys table0))
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-8
    "Initialize from a keys vector and a hash table."
    (let* ((table0 (make-random-int-hash 1024 :hash))
           (table1 (make-hash :test #'eql
                              :initial-contents (as-vector (hash-to-keys table0))
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))
  
  (test keys-9
    "Initialize from a simple key function."
    (let* ((table0 (make-hash :initial-contents '()
                              :init-data (make-hash-transformer :key #'identity)
                              :init-format :keys))
           (tablet *symbols-hash*)
           (table1 (make-hash :initial-contents (hash-to-keys tablet)
                              :init-data (make-hash-transformer :key
                                          (lambda (k) (gethash k tablet)))
                              :init-format :keys)))
      (is (zerop (hash-table-count table0)))
      (compare-hash table1 tablet)))

  (test keys-10
    "Initialize from a simple key function."
    (let* ((table0 *string-hash*)
           (trans (make-hash-transformer :key (lambda (k) (gethash k table0))))
           (table1 (make-hash :test #'equal
                              :initial-contents (hash-to-keys table0)
                              :init-data trans
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-11
    "Initialize from a keys list and an alist."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (mapcar #'car table0)
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))
  
  (test keys-12
    "Initialize from a keys list and an alist."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (mapcar #'car table0)
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-13
    "Initialize from a keys list and an alist."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (mapcar #'car table0)
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-14
    "Initialize from a keys list and an alist."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (mapcar #'car table0)
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-15
    "Initialize from a keys vector and an alist."
    (let* ((table0 *keyword-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (mapcar #'car table0))
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))
  
  (test keys-16
    "Initialize from a keys vector and an alist."
    (let* ((table0 *symbol-alist*)
           (table1 (make-hash :test #'eq
                              :initial-contents (as-vector (mapcar #'car table0))
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-17
    "Initialize from a keys vector and an alist."
    (let* ((table0 *string-alist*)
           (table1 (make-hash :test #'equal
                              :initial-contents (as-vector (mapcar #'car table0))
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-18
    "Initialize from a keys vector and an alist."
    (let* ((table0 (make-random-int-hash 1024 :alist))
           (table1 (make-hash :test #'eql
                              :initial-contents (as-vector (mapcar #'car table0))
                              :init-data table0
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-19
    "Initialize from a key function skipping some entries."
    (let* ((table0 (make-random-int-hash 1024 :hash))
           (table1 (make-hash :initial-contents (hash-to-keys table0)
                              :init-data (make-hash-transformer :key
                                          (lambda (k) (gethash k table0))
                                          #'evenp)
                              :init-format :keys))
           (table2 (make-hash-table)))
      (loop for key being each hash-key of table0 using (hash-value val)
            when (oddp key)
            do (setf (gethash key table2) val))
      (compare-hash table1 table2)))

  (test keys-20
    "Initialize from a key function defaulting some entries."
    (let* ((default-val 1001)
           (table0 (make-random-int-hash 1024 :hash))
           (table1 (make-hash :initial-contents (hash-to-keys table0)
                              :init-data (make-hash-transformer :key
                                          (lambda (k) (gethash k table0))
                                          (lambda (k) (and (evenp k) 1)))
                              :init-format :keys
                              :init-default default-val))
           (table2 (make-hash-table)))
      (loop for key being each hash-key of table0 using (hash-value val)
            do (setf (gethash key table2) (if (oddp key) val default-val)))
      (compare-hash table1 table2)))

  (test keys-21
    "Initialize from a simple key function."
    (let* ((table0 (make-hash :initial-contents (vector)
                              :test #'eql
                              :init-data (make-hash-transformer :key #'identity)
                              :init-format :keys))
           (tablet *symbols-hash*)
           (table1 (make-hash :initial-contents (as-vector (hash-to-keys tablet))
                              :test #'eql
                              :init-data (make-hash-transformer :key
                                          (lambda (k) (gethash k tablet)))
                              :init-format :keys)))
      (is (zerop (hash-table-count table0)))
      (compare-hash table1 tablet)))

  (test keys-22
    "Initialize from a simple key function."
    (let* ((table0 *string-hash*)
           (trans (make-hash-transformer :key (lambda (k) (gethash k table0))))
           (table1 (make-hash :test #'equal
                              :initial-contents (as-vector (hash-to-keys table0))
                              :init-data trans
                              :init-format :keys)))
      (compare-hash table1 table0)))

  (test keys-23
    "Initialize from a key function skipping some entries."
    (let* ((table0 (make-random-int-hash 1024 :hash))
           (table1 (make-hash :initial-contents (as-vector (hash-to-keys table0))
                              :init-data (make-hash-transformer :key
                                          (lambda (k) (gethash k table0))
                                          #'evenp)
                              :init-format :keys))
           (table2 (make-hash-table)))
      (loop for key being each hash-key of table0 using (hash-value val)
            when (oddp key)
            do (setf (gethash key table2) val))
      (compare-hash table1 table2)))

  (test keys-24
    "Initialize from a key function defaulting some entries."
    (let* ((default-val 1001)
           (table0 (make-random-int-hash 1024 :hash))
           (table1 (make-hash :initial-contents (as-vector (hash-to-keys table0))
                              :init-data (make-hash-transformer :key
                                          (lambda (k) (gethash k table0))
                                          (lambda (k) (and (evenp k) 1)))
                              :init-format :keys
                              :init-default default-val))
           (table2 (make-hash-table)))
      (loop for key being each hash-key of table0 using (hash-value val)
            do (setf (gethash key table2) (if (oddp key) val default-val)))
      (compare-hash table1 table2))))

(defsuite keychain-initializer
    "Create hash table with parallel sequences of keys and values."
  (test keychain-0
    "Initialize from an empty sequence of keys and values"
    (let* ((table1 (make-hash :initial-contents () :init-format :keychain))
           (table2 (make-hash :initial-contents (vector) :init-format :keychain)))
      (is (zerop (hash-table-count table1)))
      (is (zerop (hash-table-count table2)))))
  (test keychain-1
    "Initialize from a list of keys and a list of values."
    (let* ((table0 *symbol-alist*)
           (keys0 (mapcar #'car table0))
           (vals0 (mapcar #'cdr table0))
           (table1
            (make-hash :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-2
    "Initialize from a list of keys and a list of values."
    (let* ((table0 *keyword-alist*)
           (keys0 (mapcar #'car table0))
           (vals0 (mapcar #'cdr table0))
           (table1
            (make-hash :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))  
  (test keychain-3
    "Initialize from a list of keys and a list of values."
    (let* ((table0 *string-alist*)
           (keys0 (mapcar #'car table0))
           (vals0 (mapcar #'cdr table0))
           (table1
            (make-hash :test #'equal
                       :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-4
    "Initialize from a list of keys and a list of values."
    (let* ((table0 *string-nc-alist*)
           (keys0 (mapcar #'car table0))
           (vals0 (mapcar #'cdr table0))
           (table1
            (make-hash :test #'equalp
                       :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-5
    "Initialize from a vector of keys and a list of values."
    (let* ((table0 *symbol-alist*)
           (keys0 (as-vector (mapcar #'car table0)))
           (vals0 (mapcar #'cdr table0))
           (table1
            (make-hash :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-6
    "Initialize from a vector of keys and a list of values."
    (let* ((table0 *keyword-alist*)
           (keys0 (as-vector (mapcar #'car table0)))
           (vals0 (mapcar #'cdr table0))
           (table1
            (make-hash :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))  
  (test keychain-7
    "Initialize from a vector of keys and a list of values."
    (let* ((table0 *string-alist*)
           (keys0 (as-vector (mapcar #'car table0)))
           (vals0 (mapcar #'cdr table0))
           (table1
            (make-hash :test #'equal
                       :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-8
    "Initialize from a vector of keys and a list of values."
    (let* ((table0 *string-nc-alist*)
           (keys0 (as-vector (mapcar #'car table0)))
           (vals0 (mapcar #'cdr table0))
           (table1
            (make-hash :test #'equalp
                       :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-9
    "Initialize from a vector of keys and a vector of values."
    (let* ((table0 *symbol-alist*)
           (keys0 (as-vector (mapcar #'car table0)))
           (vals0 (as-vector (mapcar #'cdr table0)))
           (table1
            (make-hash :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-10
    "Initialize from a vector of keys and a vector of values."
    (let* ((table0 *keyword-alist*)
           (keys0 (as-vector (mapcar #'car table0)))
           (vals0 (as-vector (mapcar #'cdr table0)))
           (table1
            (make-hash :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))  
  (test keychain-11
    "Initialize from a vector of keys and a vector of values."
    (let* ((table0 *string-alist*)
           (keys0 (as-vector (mapcar #'car table0)))
           (vals0 (as-vector (mapcar #'cdr table0)))
           (table1
            (make-hash :test #'equal
                       :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-12
    "Initialize from a vector of keys and a vector of values."
    (let* ((table0 *string-nc-alist*)
           (keys0 (as-vector (mapcar #'car table0)))
           (vals0 (as-vector (mapcar #'cdr table0)))
           (table1
            (make-hash :test #'equalp
                       :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-13
    "Initialize from a list of keys and a vector of values."
    (let* ((table0 *symbol-alist*)
           (keys0 (mapcar #'car table0))
           (vals0 (as-vector (mapcar #'cdr table0)))
           (table1
            (make-hash :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-14
    "Initialize from a list of keys and a vector of values."
    (let* ((table0 *keyword-alist*)
           (keys0 (mapcar #'car table0))
           (vals0 (as-vector (mapcar #'cdr table0)))
           (table1
            (make-hash :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))  
  (test keychain-15
    "Initialize from a list of keys and a vector of values."
    (let* ((table0 *string-alist*)
           (keys0 (mapcar #'car table0))
           (vals0 (as-vector (mapcar #'cdr table0)))
           (table1
            (make-hash :test #'equal
                       :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0)))
  (test keychain-16
    "Initialize from a list of keys and a vector of values."
    (let* ((table0 *string-nc-alist*)
           (keys0 (mapcar #'car table0))
           (vals0 (as-vector (mapcar #'cdr table0)))
           (table1
            (make-hash :test #'equalp
                       :initial-contents keys0
                       :init-data vals0
                       :init-format :keychain)))
      (compare-hash table1 table0))))

(defsuite keybag-initializer
    "Create hash table from a multiset of keys and (maybe transformed) counts"
  (test keybag-0
    "Initialize from empty multiset"
    (let* ((table1 (make-hash :initial-contents () :init-format :keybag))
           (table2 (make-hash :initial-contents (vector) :init-format :keybag)))
      (is (zerop (hash-table-count table1)))
      (is (zerop (hash-table-count table2)))))
  (test keybag-1
    "Initialize from a multiset using counts, list and vector representation."
    (let* ((data0 (random-integer 0 11 1024))
           (data0v (coerce data0 'vector))
           (table0 (let ((hash (make-hash-table)))
                     (loop for key in data0 do
                           (incf (gethash key hash 0)))
                     hash))
           (table1 (make-hash :initial-contents data0  :init-format :keybag))
           (table2 (make-hash :initial-contents data0v :init-format :keybag)))
      (compare-hash table1 table0)
      (compare-hash table2 table0)))
  (test keybag-2
    "Initialize from a multiset using vector indexing."
    (let* ((outcomes #(a b c d e f g))
           (values   #(none s t u v w x y z))
           (default 'default)
           (data0  (draw-iid-sample outcomes 49))
           (data0v (coerce data0 'vector))
           ;; ATTN: this test object is rather complex and can itself have bugs
           ;; See keybag-4 for a brute force version
           (table0 (let ((hash (make-hash-table)))
                     (loop for key in data0 do
                           (incf (gethash key hash 0)))
                     (loop with len = (length values)
                           for key across outcomes
                           for index = (gethash key hash)
                           do
                           (when index
                             (setf (gethash key hash)
                                   (if (>= index len)
                                       default
                                       (aref values index)))))
                     hash))
           (table1 (make-hash :initial-contents data0
                              :init-format :keybag
                              :init-data values
                              :init-default default))
           (table2 (make-hash :initial-contents data0v
                              :init-format :keybag
                              :init-data values
                              :init-default default)))
      (compare-hash table1 table0)
      (compare-hash table2 table0)))
  (test keybag-3
    "Initialize from a multiset using hash indexing."
    (let* ((outcomes #(a b c d e f g))
           (values   (let ((hash (make-hash-table)))
                       (setf (gethash 1 hash) 's)
                       (setf (gethash 2 hash) 't)
                       (setf (gethash 3 hash) 'u)
                       (setf (gethash 4 hash) 'v)
                       (setf (gethash 5 hash) 'w)
                       (setf (gethash 6 hash) 'x)
                       (setf (gethash 7 hash) 'y)
                       (setf (gethash 8 hash) 'z)
                       hash))
           (default 'default)
           (data0  (draw-iid-sample outcomes 49))
           (data0v (coerce data0 'vector))
           ;; ATTN: this test object is rather complex and can itself have bugs
           ;; See keybag-4 for a brute force version
           (table0 (let ((hash (make-hash-table)))
                     (loop for key in data0 do
                           (incf (gethash key hash 0)))
                     (loop for key across outcomes
                           for count = (gethash key hash)
                           do
                           (when count
                             (setf (gethash key hash)
                                   (gethash count values default))))
                     hash))
           (table1 (make-hash :initial-contents data0
                              :init-format :keybag
                              :init-data values
                              :init-default default))
           (table2 (make-hash :initial-contents data0v
                              :init-format :keybag
                              :init-data values
                              :init-default default)))
      (compare-hash table1 table0)
      (compare-hash table2 table0)))
  (test keybag-4
    "Initialize from a multiset using vector indexing, brute-force version."
    (let* ((values   #(none u x y z))
           (default 'default)
           (data0  '(a b b c c c d d d d e e e e e))
           (data0v (coerce data0 'vector))
           (table0 (let ((hash (make-hash-table)))
                     (setf (gethash 'a hash) 'u)
                     (setf (gethash 'b hash) 'x)
                     (setf (gethash 'c hash) 'y)
                     (setf (gethash 'd hash) 'z)
                     (setf (gethash 'e hash) default)
                     hash))
           (table1 (make-hash :initial-contents data0
                              :init-format :keybag
                              :init-data values
                              :init-default default))
           (table2 (make-hash :initial-contents data0v
                              :init-format :keybag
                              :init-data values
                              :init-default default)))
      (compare-hash table1 table0)
      (compare-hash table2 table0)))
  (test keybag-5
    "Initialize from a multiset using vector indexing, brute-force version."
    (let* ((values  (let ((hash (make-hash-table)))
                      (setf (gethash 1 hash) 'u)
                      (setf (gethash 2 hash) 'x)
                      (setf (gethash 3 hash) 'y)
                      (setf (gethash 4 hash) 'z)
                      hash))
           (default 'default)
           (data0  '(a b b c c c d d d d e e e e e))
           (data0v (coerce data0 'vector))
           (table0 (let ((hash (make-hash-table)))
                     (setf (gethash 'a hash) 'u)
                     (setf (gethash 'b hash) 'x)
                     (setf (gethash 'c hash) 'y)
                     (setf (gethash 'd hash) 'z)
                     (setf (gethash 'e hash) default)
                     hash))
           (table1 (make-hash :initial-contents data0
                              :init-format :keybag
                              :init-data values
                              :init-default default))
           (table2 (make-hash :initial-contents data0v
                              :init-format :keybag
                              :init-data values
                              :init-default default)))
      (compare-hash table1 table0)
      (compare-hash table2 table0)))
  (test keybag-6
    "Initialize from a multiset using function indexing, brute-force version."
    (let* ((values  (lambda (key count)
                      (case count
                        (1 (values key 'u nil))
                        (2 (values key 'x nil))
                        (3 (values key 'y nil))
                        (4 (values key 'z nil))
                        (t (values key nil 1)))))
           (default 'default)
           (data0  '(a b b c c c d d d d e e e e e))
           (data0v (coerce data0 'vector))
           (table0 (let ((hash (make-hash-table)))
                     (setf (gethash 'a hash) 'u)
                     (setf (gethash 'b hash) 'x)
                     (setf (gethash 'c hash) 'y)
                     (setf (gethash 'd hash) 'z)
                     (setf (gethash 'e hash) default)
                     hash))
           (table1 (make-hash :initial-contents data0
                              :init-format :keybag
                              :init-data values
                              :init-default default))
           (table2 (make-hash :initial-contents data0v
                              :init-format :keybag
                              :init-data values
                              :init-default default)))
      (compare-hash table1 table0)
      (compare-hash table2 table0))))

(defsuite function-initializer
    "Create hash table from a function that generates entries until nil."
  (test function-0
    "Initialize from an empty genertor"
    (let* ((table1 (make-hash :initial-contents (constantly nil)
                              :init-format :function)))
      (is (zerop (hash-table-count table1)))))
  (test function-1
    "Initialize from a simple alist generator."
    (let* ((table0 *symbol-alist*)
           (alist0 table0)
           (next (lambda ()
                   (let ((item (first alist0)))
                     (setf alist0 (rest alist0))
                     (values (car item) (cdr item) nil))))
           (table1 (make-hash :initial-contents next
                              :init-format :function)))
      (compare-hash table1 table0)))
  (test function-2
    "Initialize from a simple alist generator with bad/skip keys."
    (let* ((table0a (make-random-int-hash 1024 :alist))
           (table0  (remove-if-not #'evenp table0a :key #'car))
           (alist0 table0a)
           (next (lambda ()
                   (let* ((item (first alist0))
                          (key  (car item)))
                     (setf alist0 (rest alist0))
                     (values key (cdr item) (and key (oddp key))))))
           (table1 (make-hash :initial-contents next
                              :init-format :function)))
      (compare-hash table1 table0))))

(defsuite factory-tests
    "Test the hash factory constructors."
  (test factory-0
    "Anonymous factory constructors :flat."
    (let ((hash-eq     (make-hash-factory :test #'eq     :init-format :flat))
          (hash-eql    (make-hash-factory :test #'eql    :init-format :flat))
          (hash-equal  (make-hash-factory :test #'equal  :init-format :flat))
          (hash-equalp (make-hash-factory :test #'equalp :init-format :flat))
          (table-eq     (make-hash-table  :test #'eq))
          (table-eql    (make-hash-table  :test #'eql))
          (table-equal  (make-hash-table  :test #'equal))
          (table-equalp (make-hash-table  :test #'equalp)))
      (loop for letter in '(a b c d e f g h)
            for number = 0 then (+ number 1)
            do
            (progn
              (setf (gethash letter table-eq) number)
              (setf (gethash number table-eql) letter)
              (setf (gethash (if (evenp number)
                                 (string-downcase (symbol-name letter))
                                 (string-upcase   (symbol-name letter)))
                             table-equal)
                    number)
              (setf (gethash (symbol-name letter) table-equalp) number)))
      (let ((table1
             (funcall hash-eq 'a 0 'b 1 'c 2 'd 3 'e 4 'f 5 'g 6 'h 7))
            (table2
             (funcall hash-eql 0 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6 'g 7 'h))
            (table3
             (funcall hash-equal "a" 0 "B" 1 "c" 2 "D" 3 "e" 4 "F" 5 "g" 6 "H" 7))
            (table4
             (funcall hash-equalp "a" 0 "B" 1 "c" 2 "D" 3 "e" 4 "F" 5 "g" 6 "H" 7)))
        (compare-hash table1 table-eq)
        (compare-hash table2 table-eql)
        (compare-hash table3 table-equal)
        (compare-hash table4 table-equalp))))
  (test factory-1
    "Anonymous factory constructors :pairs."
    (let ((hash-eq     (make-hash-factory :test #'eq     :init-format :pairs))
          (hash-eql    (make-hash-factory :test #'eql    :init-format :pairs))
          (hash-equal  (make-hash-factory :test #'equal  :init-format :pairs))
          (hash-equalp (make-hash-factory :test #'equalp :init-format :pairs))
          (table-eq     (make-hash-table  :test #'eq))
          (table-eql    (make-hash-table  :test #'eql))
          (table-equal  (make-hash-table  :test #'equal))
          (table-equalp (make-hash-table  :test #'equalp)))
      (loop for letter in '(a b c d e f g h)
            for number = 0 then (+ number 1)
            do
            (progn
              (setf (gethash letter table-eq) number)
              (setf (gethash number table-eql) letter)
              (setf (gethash (if (evenp number)
                                 (string-downcase (symbol-name letter))
                                 (string-upcase   (symbol-name letter)))
                             table-equal)
                    number)
              (setf (gethash (symbol-name letter) table-equalp) number)))
      (let ((table1
             (funcall hash-eq
                      '(a . 0) '(b . 1) '(c . 2) '(d . 3)
                      '(e . 4) '(f . 5) '(g . 6) '(h . 7)))
            (table2
             (funcall hash-eql
                      '(0 . a) '(1 . b) '(2 . c) '(3 . d)
                      '(4 . e) '(5 . f) '(6 . g) '(7 . h)))
            (table3
             (funcall hash-equal
                      '("a" . 0) '("B" . 1) '("c" . 2) '("D" . 3)
                      '("e" . 4) '("F" . 5) '("g" . 6) '("H" . 7)))
            (table4
             (funcall hash-equalp
                      '("a" . 0) '("B" . 1) '("c" . 2) '("D" . 3)
                      '("e" . 4) '("F" . 5) '("g" . 6) '("H" . 7))))
        (compare-hash table1 table-eq)
        (compare-hash table2 table-eql)
        (compare-hash table3 table-equal)
        (compare-hash table4 table-equalp))))
  (test factory-2
    "Named factory constructors :flat."
    (define-hash-factory test2-hash-eq     :test #'eq     :init-format :flat)
    (define-hash-factory test2-hash-eql    :test #'eql    :init-format :flat)
    (define-hash-factory test2-hash-equal  :test #'equal  :init-format :flat)
    (define-hash-factory test2-hash-equalp :test #'equalp :init-format :flat)
    (let ((table-eq     (make-hash-table  :test #'eq))
          (table-eql    (make-hash-table  :test #'eql))
          (table-equal  (make-hash-table  :test #'equal))
          (table-equalp (make-hash-table  :test #'equalp)))
      (loop for letter in '(a b c d e f g h)
            for number = 0 then (+ number 1)
            do
            (progn
              (setf (gethash letter table-eq) number)
              (setf (gethash number table-eql) letter)
              (setf (gethash (if (evenp number)
                                 (string-downcase (symbol-name letter))
                                 (string-upcase   (symbol-name letter)))
                             table-equal)
                    number)
              (setf (gethash (symbol-name letter) table-equalp) number)))
      (let ((table1
             (test2-hash-eq 'a 0 'b 1 'c 2 'd 3 'e 4 'f 5 'g 6 'h 7))
            (table2
             (test2-hash-eql 0 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6 'g 7 'h))
            (table3
             (test2-hash-equal "a" 0 "B" 1 "c" 2 "D" 3 "e" 4 "F" 5 "g" 6 "H" 7))
            (table4
             (test2-hash-equalp "a" 0 "B" 1 "c" 2 "D" 3 "e" 4 "F" 5 "g" 6 "H" 7)))
        (compare-hash table1 table-eq)
        (compare-hash table2 table-eql)
        (compare-hash table3 table-equal)
        (compare-hash table4 table-equalp))))
  (test factory-3
    "Named factory constructors :pairs."
    (define-hash-factory test3-hash-eq     :test #'eq     :init-format :pairs)
    (define-hash-factory test3-hash-eql    :test #'eql    :init-format :pairs)
    (define-hash-factory test3-hash-equal  :test #'equal  :init-format :pairs)
    (define-hash-factory test3-hash-equalp :test #'equalp :init-format :pairs)
    (let ((table-eq     (make-hash-table  :test #'eq))
          (table-eql    (make-hash-table  :test #'eql))
          (table-equal  (make-hash-table  :test #'equal))
          (table-equalp (make-hash-table  :test #'equalp)))
      (loop for letter in '(a b c d e f g h)
            for number = 0 then (+ number 1)
            do
            (progn
              (setf (gethash letter table-eq) number)
              (setf (gethash number table-eql) letter)
              (setf (gethash (if (evenp number)
                                 (string-downcase (symbol-name letter))
                                 (string-upcase   (symbol-name letter)))
                             table-equal)
                    number)
              (setf (gethash (symbol-name letter) table-equalp) number)))
      (let ((table1
             (test3-hash-eq
                      '(a . 0) '(b . 1) '(c . 2) '(d . 3)
                      '(e . 4) '(f . 5) '(g . 6) '(h . 7)))
            (table2
             (test3-hash-eql
                      '(0 . a) '(1 . b) '(2 . c) '(3 . d)
                      '(4 . e) '(5 . f) '(6 . g) '(7 . h)))
            (table3
             (test3-hash-equal
                      '("a" . 0) '("B" . 1) '("c" . 2) '("D" . 3)
                      '("e" . 4) '("F" . 5) '("g" . 6) '("H" . 7)))
            (table4
             (test3-hash-equalp
                      '("a" . 0) '("B" . 1) '("c" . 2) '("D" . 3)
                      '("e" . 4) '("F" . 5) '("g" . 6) '("H" . 7))))
        (compare-hash table1 table-eq)
        (compare-hash table2 table-eql)
        (compare-hash table3 table-equal)
        (compare-hash table4 table-equalp)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *saved-readtable* (copy-readtable)))

(defmacro restore-saved-readtable ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* *saved-readtable*)))


;; ATTN: need more of these
;; ATTN: reader config only partly testable with this framework

(install-hash-reader '() :use-dispatch t :allow-numbered-dispatch t)
(defsuite dispatch-reader-tests-A
    "Test dispatch reader factories with default settings and numbering."
  (test dispatch-0
    "Empty hash"
    (is (zerop (hash-table-count #{}))))
  (test dispatch-1
    "Simple flat, literal hash."
    (is (equalp #{a 1 b 2 c 3}
                (make-hash :initial-contents '(a 1 b 2 c 3)))))
  (test dispatch-2
    "Simple flat, literal hash using numbered dispatch."
    (is (equalp #2{"c" 1 "c" 2 "C" 3}
                (make-hash :test #'equal
                           :initial-contents '("c" 1 "c" 2 "C" 3))))
    (is (equalp #3{"c" 1 "c" 2 "C" 3}
                (make-hash :test #'equalp
                           :initial-contents '("c" 1 "C" 2 "C" 3))))))
(restore-saved-readtable)

(install-hash-reader (list :test #'equal) :use-dispatch nil)
(defsuite delimited-reader-tests-A
    "Test delimited reader factories with default settings."
  (test delimited-0
    "Empty hash"
    (is (zerop (hash-table-count #{}))))
  (test delimited-1
    "Simple flat, literal hash, eql test."
    (is (equalp {a 1 b 2 c 3}
                (make-hash :test #'equal
                           :initial-contents '(a 1 b 2 c 3)))))
  (test delimited-2
    "Simple flat, literal hash, equal test."
    (is (equalp {"c" 1 "c" 2 "C" 3}
                (make-hash :test #'equal
                           :initial-contents '("c" 1 "c" 2 "C" 3))))))
(restore-saved-readtable)

;;;; tests ends here
