;;;; make-hash -- hash table creation with flexible, extensible initializers
;;;
;;; Copyright (C) 2012 Christopher R. Genovese, all rights reserved.
;;; License: See file LICENSE.txt in this distribution.
;;;
;;; Author: Christopher Genovese <genovese@cmu.edu>
;;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;;; URL: http://github.com/genovese/make-hash
;;;
;;; Version: 1.0.1
;;; Update#: 14
;;; Created:      Wed 18 Apr 2012 at 09:55 EDT
;;; Last-Updated: Thu 05 Jul 2012 at 14:21:50 EDT
;;; Updated By: Christopher R. Genovese


(in-package #:make-hash)


;;; Error conditions

(define-condition make-hash-error (error)
  ((text :initarg :text :reader make-hash-error-text)
   (data :initarg :data :reader make-hash-error-data))
  (:documentation "Error encountered during hash table creation or initialization.")
  (:default-initargs (:text "" :data nil))
  (:report
   (lambda (c s)
     (format s (make-hash-error-text c) (make-hash-error-data c)))))

(define-condition unknown-initialization-pattern (make-hash-error)
  ()
  (:documentation "Attempt to initialize-hash with an unrecognized
pattern of argument specialization or unrecognized format."))

(define-condition unknown-default-initializer (make-hash-error)
  ()
  (:documentation "Attempt to make-hash without :init-format when no
default initialization is defined for the given initial-contents."))

(define-condition unmatched-closing-delimiter (make-hash-error)
  ()
  (:documentation "Reader can find no closing delimiter for literal
hash table."))

(define-condition numeric-dispatch-bounds-error (make-hash-error)
  ()
  (:documentation "A numeric argument to a dispatch reader
macro is out of bounds." ))


;;; Hash initialization support
;;;
;;; There are many different types and formats for the initialization of a
;;; hash table that arise in practice, and I would like to support these
;;; as naturally as possible. It is also desirable to offer flexibility
;;; that will allow users to adapt the initialization to their needs
;;; while maintaining clarity and concision. As such, the initialization
;;; is performed by generic function `initialize-hash' that uses multi-dispatch
;;; to handle the many cases modularly and extensibly. This kind of dispatching
;;; is ideally suited to CLOS, and the arguments to `initialize-hash' are
;;; specialized for each case, as described below.
;;;
;;; In addition, the generic function `hash-initializer-default-format' gives
;;; a default format (:init-format to `make-hash') to use for a given
;;; source (:initial-contents to `make-hash'), allowing easy, contingent control
;;; within an application or at the REPL.
;;;
;;; The initialization mechanism can be extended or adapted by the user by 
;;; defining or redefining methods of these two generic functions.
;;;
;;; The initialize-hash generic function takes four (specialized) arguments:
;;;
;;;    + table   -- the hash-table being initialized;
;;;    + form    -- the value of the :init-format argument to make-hash;
;;;    + source  -- principal data source for the initialization contents,
;;;                 the value of the :initial-contents argument to make-hash;
;;;    + data    -- auxilliary data for the initialization,
;;;                 the value of the :init-data argument to make-hash;
;;;    + default -- a default value when the value for a key is not available,
;;;                 the value of the :init-default argument to make-hash.
;;;
;;; The form argument is typically a keyword. Built-in support is provided for
;;; :flat, :hash, :pairs, :lists, :vectors, :seqs, :keys, :keychain, :keybag, and
;;; :function. These are described in the documentation to `make-hash'.
;;;
;;; Function arguments given as source or data are expected to return three
;;; values KEY VALUE [BAD-VALUE] that are used (under some conditions) to
;;; create a new entry KEY . VALUE in the hash to be initialized. Here,
;;; BAD-VALUE is a *ternary* value: nil means to use KEY and VALUE as is;
;;; t means to skip creating this entry; and any other non-nil object means
;;; to associate to KEY the specified *default* value instead of VALUE.
;;;
;;; In the built-in initialization support, such functions are used in one
;;; of three related ways:
;;;
;;; 1. Entry transformation:  KEY0 VALUE0 -> KEY VALUE [BAD-VALUE]
;;;                           ftype:  (function (t t) (values t t &optional t))
;;;
;;;    Given an associative map (formats :hash, :pairs, :flat, :lists, :vectors)
;;;    with entries (KEY0 . VALUE0) transformed to (KEY . VALUE) by the function.
;;;    See above for the interpretation of  bad-value, which is the same for all
;;;    three cases.
;;;
;;; 2. Key mapping:           KEY -> KEY VALUE [BAD-VALUE]
;;;                           ftype:  (function (t t) (values t &optional t))
;;;
;;;    Given a key (format :keys), this creates value from key and creates
;;;    pair (KEY . VALUE). See below for the interpretation of bad-value.
;;;
;;; 3. Entry generation:       &rest ARGS -> KEY VALUE [BAD-VALUE]
;;;                           ftype (function (&rest t) (values t t &optional t))
;;;
;;;    Each call produces a new (KEY . VALUE) pair subject to two provisos,
;;;    i) the interpretation of bad-value, and ii) if the returned key is nil
;;;    then generation halts.
;;;
;;; All three signatures are supported by the following construct.
;;; 

(defmacro set-transformed-entry (entry0 default via transform into table 
                                 &optional on condition do)
  "Create a new hash table entry from a given entry and transform protocol.
ENTRY0 specifies the information to which the transform is apply'd.
DEFAULT is a default value to use for entry if the transform indicates
so. TRANSFORM is a function that accepts arguments in the entry (e.g., a
key value pair) and returns three values a KEY, a VALUE, and a BAD-VALUE
ternary indicator, with the last of these optional. If BAD-VALUE is nil,
KEY and VALUE will be entered in TABLE; if BAD-VALUE is t, the entry is
skipped; and BAD-VALUE is otherwise non-nil, KEY and DEFAULT are entered
into TABLE. TABLE is the hash table to modify.

If ON is supplied (it's value does not matter), then the function
CONDITION is called on the KEY returned by TRANSFORM, and the form DO is
executed if this returns a non-nil value. This occurs *before* the
BAD-VALUE is checked or the key assigned.

VIA and INTO are syntactic placeholders whose values are ignored."
  (declare (ignore into via))
  (let ((key (gensym "key"))
        (val (gensym "val"))
        (bad (gensym "bad")))
    `(multiple-value-bind (,key ,val ,bad)
         ,(if (listp entry0)
              `(funcall ,transform ,@entry0)
              `(apply ,transform ,entry0))
       ,@(if on (list (list 'when (list condition key) do)) nil)
       (unless (and ,bad (eq ,bad t))
         (setf (gethash ,key ,table) (if ,bad ,default ,val))))))


(defgeneric initialize-hash (table form source data default)
  (:documentation
   "Create and add entry to TABLE using info of format FORM in SOURCE and DATA.
SOURCE contains the main contents, and DATA contains auxilliary
information or objects required for initialization. DEFAULT is the value
that should be stored in the table when an appropriate value associated
to a key cannot be found. See `make-hash' for details on configurations
with predefined support. Adding or redefining methods for this function
allows extension or modification of the initialization mechanism.

Note the convention, used by the predefined methods, that functions
passed as either SOURCE or DATA are expected to return three values

    KEY VALUE [BAD-VALUE] 

that are used (under certain conditions) to create a new entry KEY .
VALUE in the TABLE to be initialized. Here, BAD-VALUE is a *ternary*
value: nil means to use KEY and VALUE as is; t means to skip creating
this entry; and any other non-nil object means to associate to KEY the
specified DEFAULT instead of VALUE."))


;; Unspecialized method to catch unsupported configurations

(defmethod initialize-hash ((table t) (form t) (source t) (data t) default)
  "Fallback initialization of unsupported configurations."
  (let ((mesg
         (format nil "Unsupported signature (~{~A~^ ~}) for initialize-hash"
                 (mapcar (lambda (u)
                           (if (keywordp u) u (class-name (class-of u))))
                         (list table form source data default)))))
    (error 'unknown-initialization-pattern
           :text mesg
           :data (list table form source data default))))


;; Initialize from existing hash table either shallow copy or transformed

(defmethod initialize-hash 
    ((table hash-table) (form (eql :hash)) (source hash-table) (data null) default)
  "Make TABLE a shallow copy of SOURCE with shared key and value structure."
  (loop for key being each hash-key of source using (hash-value val)
        do (setf (gethash key table) val)))

(defmethod initialize-hash
    ((table hash-table) (form (eql :hash)) (source hash-table) (data function) default)
  "Make TABLE a transformed copy of SOURCE by function DATA, possibly sharing structure."
  (loop for key0 being each hash-key of source using (hash-value val0)
        do (set-transformed-entry (key0 val0) default via data into table)))


;; Initialize from flat sequence <key value ....>, optionally transforming entries

(defmethod initialize-hash 
    ((table hash-table) (form (eql :flat)) (source list) (data null) default)
  "SOURCE is a list of alternating key-value pairs that are entered into TABLE."
  (loop for keyval on source by #'cddr do
        (setf (gethash (first keyval) table) (second keyval))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :flat)) (source list) (data function) default)
  "SOURCE is a list of alternating key-value pairs that are transformed
by the function DATA and entered into TABLE."
  (loop for keyval on source by #'cddr do
        (set-transformed-entry ((first keyval) (second keyval)) default
                               via data into table)))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :flat)) (source vector) (data null) default)
  "SOURCE is a vector of alternating key-value pairs that are entered into TABLE."
  (loop for i from 0 below (length source) by 2 do
        (setf (gethash (aref source i) table) (aref source (+ i 1)))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :flat)) (source vector) (data function) default)
  "SOURCE is a vector of alternating key-value pairs that are transformed by the 
function DATA and entered into TABLE."
  (loop for i from 0 below (length source) by 2
        for key0 = (aref source i)
        for val0 = (aref source (+ i 1))
        do (set-transformed-entry (key0 val0) default via data into table)))


;; Initialize from existing alist (or avector), optionally transforming entries

(defmethod initialize-hash 
    ((table hash-table) (form (eql :pairs)) (source list) (data null) default)
  "SOURCE is an alist whose key-value pairs are entered into TABLE."
  (dolist (entry source)
    (setf (gethash (car entry) table) (cdr entry))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :pairs)) (source list) (data function) default)
  "SOURCE is an alist whose key-value pairs are transformed by function DATA and 
entered into TABLE."
  (dolist (entry source)
    (set-transformed-entry ((car entry) (cdr entry)) default
                           via data into table)))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :pairs)) (source vector) (data null) default)
  "SOURCE is a vector of (key . value) cons pairs that are entered into TABLE."
  (loop for entry across source do
        (setf (gethash (car entry) table) (cdr entry))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :pairs)) (source vector) (data function) default)
  "SOURCE is a vector of (key . value) pairs that are transformed by function
DATA and entered into TABLE."
  (loop for entry across source do
        (set-transformed-entry ((car entry) (cdr entry)) default
                               via data into table)))


;; Initialize from sequence of lists, optionally transforming entries

(defmethod initialize-hash 
    ((table hash-table) (form (eql :lists)) (source list) (data null) default)
  "SOURCE is a list of lists of the form (key val ...), and each key-val pair
is entered into TABLE."
  (dolist (entry source)
    (setf (gethash (first entry) table) (second entry))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :lists)) (source list) (data function) default)
  "SOURCE is a list of lists of the form (key val ...), and each key-val pair
is transformed by DATA and entered into TABLE."
  (dolist (entry source)
    (set-transformed-entry ((first entry) (second entry)) default
                           via data into table)))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :lists)) (source vector) (data null) default)
  "SOURCE is a vector of lists of the form (key val ...), and each key-val pair
is transformed by DATA then entered into TABLE."
  (loop for entry across source do
        (setf (gethash (first entry) table) (second entry))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :lists)) (source vector) (data function) default)
  "SOURCE is a vector of lists of the form (key val ...), and each key-val pair
is transformed by DATA and entered into TABLE."
  (loop for entry across source do
        (set-transformed-entry ((first entry) (second entry)) default
                               via data into table)))


;; Initialize from sequence of vectors, optionally transforming entries

(defmethod initialize-hash 
    ((table hash-table) (form (eql :vectors)) (source list) (data null) default)
  "SOURCE is a list of lists of the form (key val ...), and each key-val pair
is entered into TABLE."
  (dolist (entry source)
    (setf (gethash (aref entry 0) table) (aref entry 1))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :vectors)) (source list) (data function) default)
  "SOURCE is a list of vectors of the form [key val ...], and each key-val pair
is transformed by DATA and entered into TABLE."
  (dolist (entry source)
    (set-transformed-entry ((aref entry 0) (aref entry 1)) default
                           via data into table)))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :vectors)) (source vector) (data null) default)
  "SOURCE is a vector of lists of the form (key val ...), and each key-val pair
is transformed by DATA then entered into TABLE."
  (loop for entry across source do
        (setf (gethash (aref entry 0) table) (aref entry 1))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :vectors)) (source vector) (data function) default)
  "SOURCE is a vector of lists of the form (key val ...), and each key-val pair
is transformed by DATA and entered into TABLE."
  (loop for entry across source do
        (set-transformed-entry ((aref entry 0) (aref entry 1)) default
                               via data into table)))


;; Initialize from sequence of sequences, optionally transforming entries

(defmethod initialize-hash 
    ((table hash-table) (form (eql :seqs)) (source list) (data null) default)
  "SOURCE is a list of sequences of the form (key val ...), and each key-val pair
is entered into TABLE."
  (dolist (entry source)
    (setf (gethash (elt entry 0) table) (elt entry 1))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :seqs)) (source list) (data function) default)
  "SOURCE is a list of sequences of the form [key val ...], and each key-val pair
is transformed by DATA and entered into TABLE."
  (dolist (entry source)
    (set-transformed-entry ((elt entry 0) (elt entry 1)) default
                           via data into table)))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :seqs)) (source vector) (data null) default)
  "SOURCE is a vector of sequences of the form (key val ...), and each key-val pair
is transformed by DATA then entered into TABLE."
  (loop for entry across source do
        (setf (gethash (elt entry 0) table) (elt entry 1))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :seqs)) (source vector) (data function) default)
  "SOURCE is a vector of sequences of the form (key val ...), and each key-val pair
is transformed by DATA and entered into TABLE."
  (loop for entry across source do
        (set-transformed-entry ((elt entry 0) (elt entry 1)) default
                               via data into table)))


;; Initialize from given keys and another associative map (alist or hash)

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keys)) (source list) (data hash-table) default)
  "SOURCE is a list of keys, DATA is a hash table whose corresponding entries are stored in TABLE."
  (loop for key in source
        do (setf (gethash key table) (gethash key data default))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keys)) (source list) (data list) default)
  "SOURCE is a list of keys, DATA is an alist whose corresponding entries are stored in TABLE."
  (loop for key in source
        for entry = (assoc key data :test (hash-table-test table))
        do (setf (gethash key table) (if entry (cdr entry) default))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keys)) (source list) (data function) default)
  "SOURCE is a list of keys, DATA is an function mapping keys to entries stored in TABLE."
  (loop for key in source
        do (set-transformed-entry (key) default via data into table)))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keys)) (source vector) (data hash-table) default)
  "SOURCE is a vector of keys, DATA is a hash table whose corresponding entries are stored in TABLE."
  (loop for key across source
        do (setf (gethash key table) (gethash key data default))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keys)) (source vector) (data list) default)
  "SOURCE is a vector of keys, DATA is an alist whose corresponding entries are stored in TABLE."
  (loop for key across source
        for entry = (assoc key data :test (hash-table-test table))
        do (setf (gethash key table) (if entry (cdr entry) default))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keys)) (source vector) (data function) default)
  "SOURCE is a vector of keys, DATA is an function mapping keys to entries stored in TABLE."
  (loop for key across source
        do (set-transformed-entry (key) default via data into table)))


;; Initialize from parallel ordered sequences of keys and values

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keychain)) (source list) (data list) default)
  "SOURCE is a list of keys, DATA is a parallel list of values; pairs stored in TABLE.
If SOURCE is longer than DATA, additional entries use DEFAULT for their value."
  (loop for key in source
        for val-tail = data then (cdr val-tail)
        do (setf (gethash key table) (if val-tail (car val-tail) default))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keychain)) (source vector) (data list) default)
  "SOURCE is a vector of keys, DATA is a parallel list of values; pairs stored in TABLE.
If SOURCE is longer than DATA, additional entries use DEFAULT for their value."
  (loop for key across source
        for val-tail = data then (cdr val-tail)
        do (setf (gethash key table) (if val-tail (car val-tail) default))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keychain)) (source list) (data vector) default)
  "SOURCE is a list of keys, DATA is a parallel vector of values; pairs stored in TABLE.
If SOURCE is longer than DATA, additional entries use DEFAULT for their value."
  (loop with n = (length data)
        for key in source
        for i = 0 then (+ i 1)
        do (setf (gethash key table) (if (< i n) (aref data i) default))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keychain)) (source vector) (data vector) default)
  "SOURCE is a vector of keys, DATA a parallel vector of values; pairs stored in TABLE.
If SOURCE is longer than DATA, additional entries have value DEFAULT."
  (loop with n = (length data)
        for key across source
        for i = 0 then (+ i 1)
        do (setf (gethash key table) (if (< i n) (aref data i) default))))


;; Initialize from a bag (multiset) of keys, where the counts determine the values

(defmacro bag-seq-to-hash (seq seqtype hash)
  "Convert a sequence representation of a bag/multiset to a hash table.
SEQ is a sequence of type SEQTYPE, a symbol, either list or vector explicitly.
Its entries are elements of the base set, possibly with repetitions.
HASH is a hash table. Upon return, its keys are the elements of the base
set, and its values are the repetition counts for each element."
  (declare (type (member list vector) seqtype))
  (let ((key (gensym "KEY")))
    `(loop for ,key ,(if (eq seqtype 'list) 'in 'across) ,seq
           do (incf (gethash ,key ,hash 0)))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keybag)) (source list) (data null) default)
  "SOURCE is a list of keys as a bag/multiset, counts stored in TABLE for each key.
DEFAULT is ignored."
  (bag-seq-to-hash source list table))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keybag)) (source vector) (data null) default)
  "SOURCE is a vector of keys as a bag/multiset, counts stored in TABLE for each key.
DEFAULT is ignored."
  (bag-seq-to-hash source vector table))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keybag)) (source list) (data vector) default)
  "SOURCE is a list of keys, element of DATA at index of key's count is key's value.
If count is >= length of DATA vector, DEFAULT is used instead."
  (let ((counts
         (make-hash-table :test (hash-table-test table)
                          :size (hash-table-size table)
                          :rehash-size (hash-table-rehash-size table)
                          :rehash-threshold (hash-table-rehash-threshold table))))
    (bag-seq-to-hash source list counts)
    (loop with n = (length data)
          for key being each hash-key of counts using (hash-value count) do
          (setf (gethash key table)
                (if (>= count n) default (aref data count))))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keybag)) (source vector) (data vector) default)
  "SOURCE is a vector of keys, element of DATA at index of key's count is key's value.
If count is >= length of DATA vector, DEFAULT is used instead."
  (let ((counts
         (make-hash-table :test (hash-table-test table)
                          :size (hash-table-size table)
                          :rehash-size (hash-table-rehash-size table)
                          :rehash-threshold (hash-table-rehash-threshold table))))
    (bag-seq-to-hash source vector counts)
    (loop with n = (length data)
          for key being each hash-key of counts using (hash-value count) do
          (setf (gethash key table)       
                (if (>= count n) default (aref data count))))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keybag)) (source list) (data hash-table) default)
  "SOURCE is a list of keys, value in DATA for key's count is key's value.
If count is not in DATA table, DEFAULT is used instead."
  (let ((counts
         (make-hash-table :test (hash-table-test table)
                          :size (hash-table-size table)
                          :rehash-size (hash-table-rehash-size table)
                          :rehash-threshold (hash-table-rehash-threshold table))))
    (bag-seq-to-hash source list counts)
    (loop for key being each hash-key of counts using (hash-value count) do
          (setf (gethash key table) (gethash count data default)))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keybag)) (source vector) (data hash-table) default)
  "SOURCE is a list of keys, value in DATA for key's count is key's value.
If count is not in DATA table, DEFAULT is used instead."
  (let ((counts
         (make-hash-table :test (hash-table-test table)
                          :size (hash-table-size table)
                          :rehash-size (hash-table-rehash-size table)
                          :rehash-threshold (hash-table-rehash-threshold table))))
    (bag-seq-to-hash source vector counts)
    (loop for key being each hash-key of counts using (hash-value count) do
          (setf (gethash key table) (gethash count data default)))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keybag)) (source list) (data function) default)
  "SOURCE is a vector of keys, DATA maps keys and counts to entries stored in TABLE."
  (let ((counts
         (make-hash-table :test (hash-table-test table)
                          :size (hash-table-size table)
                          :rehash-size (hash-table-rehash-size table)
                          :rehash-threshold (hash-table-rehash-threshold table))))
    (bag-seq-to-hash source list counts)
    (loop for key being each hash-key of counts using (hash-value count) do
          (set-transformed-entry (key count) default via data into table))))

(defmethod initialize-hash 
    ((table hash-table) (form (eql :keybag)) (source vector) (data function) default)
  "SOURCE is a vector of keys, DATA maps keys and counts to entries stored in TABLE."
  (let ((counts
         (make-hash-table :test (hash-table-test table)
                          :size (hash-table-size table)
                          :rehash-size (hash-table-rehash-size table)
                          :rehash-threshold (hash-table-rehash-threshold table))))
    (bag-seq-to-hash source vector counts)
    (loop for key being each hash-key of counts using (hash-value count) do
          (set-transformed-entry (key count) default via data into table))))


;; Initialize from repeated calls to a function returning entries until nil

(defmethod initialize-hash 
    ((table hash-table) (form (eql :function)) (source function) (data list) default)
  "SOURCE is a function returning entries in TABLE until primary value is nil.
DATA is a list of arguments, possibly null, passed to the function each call."
  (loop named iteration do
        (set-transformed-entry data default via source into table
                               at null (return-from iteration))))



;; DWIM Default Initializer Format

(defgeneric hash-initializer-default-format (source)
  (:documentation
   "Select an initializer format based on the given initial contents SOURCE."))

(defmethod hash-initializer-default-format ((source hash-table))
  :hash)

(defmethod hash-initializer-default-format ((source list))
  :flat)

(defmethod hash-initializer-default-format ((source vector))
  :flat)

(defmethod hash-initializer-default-format ((source function))
  :function)

(defmethod hash-initializer-default-format ((source t))
  ; TODO: define restarts to allow the user to choose a format.
  (let ((mesg
         (format nil "No default initializer for source of class ~A"
                 (class-name (class-of source)))))
    (error 'unknown-default-initializer :text mesg :data source)))



;; Main Exported Entry Point

(defun make-hash (&rest hash-options &key
                        (initial-contents nil)
                        (init-format
                         (hash-initializer-default-format initial-contents))
                        (init-data nil)
                        (init-default nil)
                        &allow-other-keys)
  "Create, initialize, and return a new hash table.

Keyword options include all those of the standard `make-hash-table', any
extension options allowed by the given implementation, and the following
four additional keyword arguments to control initialization. Users can
support other types/configurations (or alter the default handling) by
extending the generic function `initialize-hash' in this package.

  :INITIAL-CONTENTS object

    If the supplied object is non-nil, the object is used to initialize
    the created hash table in accordance with the INIT-FORMAT argument.
    For some formats, the INIT-DATA argument may also be needed to
    supply supplementary information for the initializer. The built-in
    formats support the cases where object is either a hash table or
    sequence from which the keys and values can be extracted. See the
    table below for a detailed description of the possibilities.

  :INIT-FORMAT keyword

    A keyword specifying the structure of the initialization contents
    and auxilliary data given by the INITIAL-CONTENTS and INIT-DATA
    arguments. Built-in support is provided for :hash, :flat, :pairs, 
    :lists, :vectors, :seqs, :keys, :keychain, :keybag, and :function.
    These are described in detail below. When an initializer format is
    not supplied, it is computed by calling the generic function
    `hash-initializer-default-format' on the given INITIAL-CONTENTS
    object. A methods for this function should be defined whenever the
    function `initialize-hash' is extended to handle a new class of
    INITIAL-CONTENTS objects. Methods can be overridden to change the
    default used in existing cases.

  :INIT-DATA object

    Auxilliary data used for initialization. Its structure and meaning 
    depends on the value of INIT-FORMAT; see below for details.

  :INIT-DEFAULT value

    Default value to use in indirect initialization when the value for 
    the given key cannot be determined from the INITIAL-CONTENTS and
    INIT-DATA for the particular INIT-FORMAT supplied.

Note that in cases where INITIAL-CONTENTS or INIT-DATA is a function,
that function should return three values. The primary value is the
key to set in the hash table; the secondary value is the value associated
with that key; and the optional ternary value is an indicator of whether
the key's value or entry should be used. For the latter, `nil' means that
the returned key and value should be stored in the table; `t' means that
the entry should be *skipped*; and any other non-nil value means that
the key should be used with the supplied default value.

For the built-in initialization methods, such functions are used in three
ways: entry transformation -- taking a key and value as arguments, key 
mapping -- taking just a key as argument, and entry generation -- taking 
arbitrary &rest arguments and returning nil when iteration should stop.
See below for more detail.

The following table describes the built-in initialization formats and
how the INITIAL-CONTENTS and INIT-DATA arguments are interpreted.
Either vectors or lists can be used interchangeably for passing sequences 
to the latter arguments, except when data is an alist with :keys and a 
list of arguments with :function. But in those latter cases, the user
can coerce a vector to list first if necessary.

  Format     Contents    Data       Description
  ---------  --------    ----       -----------
  :hash      hash-table  null       Shallow copy of given hash table with
                                    shared structure in keys and values.
                         function   Entry transformation of given hash table,
                                    shared structure is possible.
                                    
  :flat      list or     null       List or vector of alternating keys and values
             vector                 (key1 val1 ...) or #(key1 val1 ...), 
                                    in the style of a plist.
                         function   Entry transformation of supplied
                                    keys and values via given function
                                    
  :pairs     list or     null       List or vector of (key . value) cons pairs,
             vector                 ((key1 . val1) ...) or #((key1 . val1)...).
                         function   Entry transformation of supplied
                                    key-value pairs via given function
                                    
  :lists     list or     null       List or vector of (key value) lists,
             vector                 with only the first two elements of each used
                         function   Entry transformation of supplied
                                    keys and values via given function
                                    
  :vectors   list or     null       List or vector of #(key value) vectors,
             vector                 with only the first two elements of each used
                         function   Entry transformation of supplied
                                    keys and values via given function
                                    
  :seqs      list or     null       List or vector of [key value] sequences,
             vector                 with each sequence either a list or vector
                                    and only the first two elements of each used
                         function   Entry transformation of supplied
                                    keys and values via given function
           
  :keys      list or     hash-table Contents is a list or vector of keys that 
             vector                 are looked up in the hash-table data. These
                                    key-value pairs are used for initialization, 
                                    with keys that are not found associated with 
                                    the given default.

             list or     list       Contents is a list or vector of keys that 
             vector                 are looked up in the alist data. These
                                    key-value pairs are used for initialization, 
                                    with keys that are not found associated with 
                                    the given default.

             list or     function   Contents is a list or vector of keys that 
             vector                 are passed to the function data for key 
                                    mapping. The resulting key-value pairs 
                                    (allowing skips or defaults via bad-value)
                                    are used for initialization. 

  :keychain  list or     list or    Contents is a list or vector of keys, and
             vector      vector     data is a parallel list or vector of values
                                    given *in the same order*. Corresponding
                                    key-value pairs are used in initialization.

  :keybag    list or     null       Contents is a bag/multiset represented
             vector                 as a list or vector. The hash table is
                                    initialized to associate each unique key
                                    to its count in the multiset.

             list or     vector     Contents is a bag/multiset represented
             vector                 as a list or vector. Data is a vector to
                                    be indexed by the counts of each key.
                                    Hash table is initialized to associate
                                    each key to the value in data at index
                                    equal to that key's count. Counts outside
                                    the bounds of data are associated to 
                                    the default value. 

            list or     hash-table  Contents is a bag/multiset represented
             vector                 as a list or vector. Data is a hash-table
                                    with positive integer keys representing a
                                    sparse vector. Hash table is initialized to 
                                    associate each key to the value in data that
                                    is associated with that key's count. Counts 
                                    not in data are associated to the default 
                                    value. 

            list or     function    Contents is a bag/multiset represented
             vector                 as a list or vector. Data is a function
                                    of two arguments KEY and COUNT, the latter
                                    a positive integer. Table is initialized to 
                                    associate each key to the value returned
                                    by data on that key and its count. Data
                                    satisfies the bad-value convention described
                                    earlier. 

  :function function    list or     Contents is a function that is applied to
                        null        the values in the list data. Hash table
                                    is initialized by entry generation until
                                    the function returns a nil primary value.
"
  (let* ((hash (apply #'make-hash-table :allow-other-keys t hash-options)))
    (when initial-contents
      (initialize-hash
       hash init-format initial-contents init-data init-default))
    hash))


;; Convenient way to specify transformation in function-based initialization

(defun make-hash-transformer (domain f &optional (badp (constantly nil)))
  "Transform function on DOMAIN, F, to be suitable for use with `make-hash'.
DOMAIN is one of the keywords :key, :value, or :entry. F is a function
that takes a key, a value, or a key and a value, respectively. BADP is a
function with the same argument signature that returns a ternary value:
nil means that the transformed entry should be used as is, t means that
the entry should be skipped, and any other non-nil value means that the
key should be used with a default. Note that if BADP returns a non-nil
value, then F is *not* called for that entry.

The returned function accepts a key and a value (the value is optional
with DOMAIN :key) and returns three values: the key, the value, and the
bad-value ternary for that entry. This has a signature appropriate for
passing as the :initial-contents or :init-data arguments to `make-hash'
when the format expects/allows a function in those slots."
  (declare (type (member :key :value :entry) domain)
           (type function f badp))
  (ecase domain
    (:key
     (lambda (key &optional val)
       (declare (ignorable val))
       (let ((bad? (funcall badp key)))
         (values key (if bad? nil (funcall f key)) bad?))))
    (:value
     (lambda (key val)
       (let ((bad? (funcall badp val)))
         (values key (if bad? nil (funcall f val)) bad?))))
    (:entry
     (lambda (key val)
       (let ((bad? (funcall badp key val)))
         (values key (if bad? nil (funcall f key val)) bad?))))))


;; Hash factory constructors
;;
;; It can be useful to have a shortcut for hash-table creation when
;; making many literal hashes of small-to-moderate size with one or a
;; few initialization policies. The routines here provide such a
;; mechanism. Both `define-hash-factory' and `make-hash-factory' return
;; functions, named and anonymous respectively, that package their
;; arguments as the initial contents in a call to `make-hash'. The
;; options passed to `make=hash' can be specified when the factory
;; is created but are otherwise taken from the dynamic variable
;; `*hash-factory-defaults*'. For example, we can do the following:
;;
;;   (define-hash-factory hash :test #'equal :init-format :flat)
;;   (define-hash-factory hash-pairs :test #'eql  :init-format :pairs)
;;   
;;   (do-something-with-hash
;;       (hash "A" 1 "B" 2 :C 3))
;;   
;;   (do-something-with-hash
;;       (hash-pairs '(a . "aardvaark") '(b . "Borges") '(c . "cerulean")))
;;
;;   (let ((subhash-big
;;          (make-hash-factory :init-format :keys :init-data some-big-hash)))
;;     (in-some-loop-with-some-variables
;;      (let ((X (funcall subhash-big key-u key-v key-w key-x key-y key-z)))
;;        (do-stuff-with-hash X))))
;;
;; For simple things and often at the REPL, this can be clearer and
;; more convenient, though the generality of `make-hash' may be needed
;; in most cases.
 
(defvar *hash-factory-defaults*
  (list
   :test             #'eql
   :init-format      :flat
   :init-data        nil
   :init-default     nil
   :size             127
   :rehash-size      1.5
   :rehash-threshold 1)
  "Hash table creation options used as defaults by hash factory constructors.
These option specifications are passed last to make-hash by the hash
factories and so are overridden by options passed as explicit arguments
to the factory constructor.

Changing this variable affects the options used by every hash factory
that does not fully specify its options. This includes default calls to
the reader constructors. Of particular note are the :test
and :init-format options.")

(defmacro define-hash-factory (name &rest hash-options &key &allow-other-keys)
  "Create a hash-table factory NAME that calls `make-hash' with HASH-OPTIONS.
The resulting function packages its arguments as a list, which it passes
as the :initial-contents argument to `make-hash'. The HASH-OPTIONS are
alternating keywords and values that are passed as additional keyword
arguments to `make-hash', followed by -- and thus overriding -- the
options in `*hash-factory-defaults*'. This is intended to allow one to
use short names or customized policies in simple calling patterns.
Complex initialization patterns may need the full power of `make-hash'
itself."
  (declare (type symbol name))
  (let ((hash-contents (gensym "contents")))
    `(defun ,name (&rest ,hash-contents)
       (apply #'make-hash
              :initial-contents ,hash-contents
              ,@hash-options *hash-factory-defaults*))))

(defun make-hash-factory (&rest hash-options &key &allow-other-keys)
  "Create anonymous hash-table factory that calls `make-hash' with HASH-OPTIONS.
The resulting function packages its arguments as a list, which it passes
as the :initial-contents argument to `make-hash'. The HASH-OPTIONS are
alternating keywords and values that are passed as additional keyword
arguments to `make-hash', followed by -- and thus overriding -- the
options in `*hash-factory-defaults*'. This is intended to allow one to
use short names or customized policies in simple calling patterns.
Complex initialization patterns may need the full power of `make-hash'
itself."
  (lambda (&rest hash-contents)
    (apply #'make-hash
           :initial-contents hash-contents
           (append hash-options *hash-factory-defaults*))))


;; Reader Support
;;
;; As with the hash factory commentary above, it can often be pleasant
;; and useful to have a reader-macro for representing hash-tables factories,
;; analogous to those for lists and vectors. The functions below,
;; `install-hash-reader' and `install-hash-dispatch-reader', provide two
;; portable ways of setting those macros in a given readtable, named
;; or otherwise. The former defines terminating macro characters to
;; act as delimiters for the hash factory, such as { ... }. The latter
;; is analogous except that it requires a dispatch character first,
;; # by default, such as #{ ... }. Typically, only one would be used for
;; any particular readtable, but it is flexible.

(defun read-close-delimiter (stream char)
  (error 'unmatched-closing-delimiter
         :text "Unmatched closing ~A on stream ~A"
         :data (list (substitute #\Space #\_ (string-downcase (char-name char)))
                     stream)))

;; ATTN change install-hash-reader to install-hash-delimited-reader
;; and then make install-hash-reader a macro that expands to an
;; eval-when.  Add a :hash-factory argument (nil by default)
;; for constructing the main hash, add a :use-dispatch argument
;; (t by default) that determines whether delimited or dispatch version
;; is used. Change :custom-test to :alternative-test
;; Add a dispatch arg table argument :dispatch-arg-table that
;; takes the number after the dispatch
;; character and gets the factory from that or uses alternative-test
;; if the number is too high, or default if no number.  Set up a special
;; variable with the default case
;; ATTN: Could put the dispatch arg table as the argument to use-dispatch
;; it should be a vector and it will be used for getting the factory
;; from numeric arguments.  (Is this worth supporting??)
;; This gets rid of the dispatch-arg-table keyword which is annoying.
;; Consider getting rid of alternate-test then because that can be
;; included in the table

(defvar *numbered-dispatch-defaults*
  (vector #'eq #'eql #'equal #'equalp)
  "Hash table tests used by dispatch reader macros with numeric arguments.
A numeric argument of n to the reader macro uses the test at index n,
but an index longer than the length of this sequence raises an error.")

(defmacro install-hash-reader (options
                               &key (readtable '*readtable*)
                                    (use-dispatch t)
                                    (allow-numbered-dispatch nil)
                                    (open-char #\{) (close-char #\})
                                    (dispatch-char #\#))
  "Creates a hash table factory specified by OPTIONS and installs it in
   READTABLE (the current readtable by default). To have effect, this
   must be called at toplevel.

   OPTIONS is either a list of keyword-value pairs (as would be passed to
   `make-hash' or `make-hash-factory') or a hash factory function.
   READTABLE is a readtable object, `*readtable*' by default.

   The keyword arguments control how the reader is modified as follows:

   + USE-DISPATCH (t by default) determines whether the reader macro uses a
     dispatch character DISPATCH-CHAR before OPEN-CHAR. If non-nil, a
     dispatch character is used and is registered in READTABLE. If this is
     nil, then OPEN-CHAR and CLOSE-CHAR will be a raw delimited construct.
   
   + ALLOW-NUMBERED-DISPATCH (nil by default) allows a dispatched reader
     macro to modify its hash test when given numeric arguments between
     DISPATCH-CHAR and OPEN-CHAR. This only applies when USE-DISPATCH is
     non-nil and when OPTIONS is a list, not a factory function. The goal
     here is to make it easy to reuse reader factories in several contexts.
     
     If nil, numbered dispatch is not supported. If t, numeric arguments
     0, 1, 2, and 3 correspond to hash tests `eq', `eql', `equal', and 
     `equalp' respectively. If a sequence of symbols or functions, 
     those functions are used for the hash test given a numeric
     argument from 0 below the length of the sequence. In either case,
     dispatch without a numeric argument uses the originally specified
     options.

     Note: This is an experimental feature and may be discontinued in
     future versions if it proves more confusing than helpful.

   + OPEN-CHAR (default open-brace) is the character that delimits the
     beginning of the hash-table contents. If USE-DISPATCH is non-nil,
     this character must be preceeded by DISPATCH-CHAR, and optionally
     a numeric argument.

   + CLOSE-CHAR (default close-brace) is the character that delimits
     the end of the hash-table contents.

   + DISPATCH-CHAR (default #) is the character used to indicate a
     dispatched reader macro. When (and only when) USE-DISPATCH is non-nil.
     READTABLE is modified to register this as as a dispatch and a
     non-terminating macro character via `make-dispatch-macro=character'.
     Note that there can be more than one dispatch character in a read
     table."
  (flet ((do-dispatch (numtests)
           `(install-hash-dispatch-reader ,options ,readtable ,dispatch-char
                                          ,open-char ,close-char ,numtests))
         (do-delimited ()
           `(install-hash-delimited-reader ,options ,readtable
                                           ,open-char ,close-char)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(cond
         ((eq use-dispatch t)
          (do-dispatch (if (eq allow-numbered-dispatch t)
                           '*numbered-dispatch-defaults*
                           allow-numbered-dispatch)))
         ((null use-dispatch)
          (do-delimited))
         (t ; general case, but uncommon; why clutter the output unnecessarily
          (let ((dispatch (gensym "use-dispatch"))
                (numargs  (gensym "allow-numargs"))
                (argvec   (gensym "argvec")))
            `(let* ((,dispatch ,use-dispatch)
                    (,numargs (and ,dispatch ,allow-numbered-dispatch))
                    (,argvec (if (eq ,numargs t)
                                 *numbered-dispatch-defaults* ,numargs)))
               (if ,dispatch
                   ,(do-dispatch argvec)
                   ,(do-delimited)))))))))

(defun install-hash-delimited-reader (options readtable open-char close-char)
  (declare (type character open-char close-char))
  (let ((hash-factory (apply #'make-hash-factory options)))
    (flet ((hash-reader (stream char)
             (declare (ignore char))
             (apply hash-factory (read-delimited-list close-char stream t))))
      (set-macro-character open-char #'hash-reader nil readtable)
      (set-macro-character close-char #'read-close-delimiter nil readtable))))

(defun install-hash-dispatch-reader (options readtable dispatch-char
                                     open-char close-char numeric-arg-table)
  (let ((factory (apply #'make-hash-factory options))
        (alt-factories (map 'vector
                            #'(lambda (test)
                                (apply #'make-hash-factory :test test options))
                            numeric-arg-table)))
    (flet ((hash-dispatch-reader (stream char num)
             (declare (ignore char))
             (let ((create
                    (cond
                      ((or (null num) (not (numberp num)))
                       factory)
                      ((< num (length alt-factories))
                       (aref alt-factories num))
                      (t
                       (error 'numeric-dispatch-bounds-error
                              :text "Numeric argument to dispatch reader-macro out of bounds."
                              :data `(>= ,num ,(length alt-factories)))))))
               (apply create (read-delimited-list close-char stream t)))))
      (handler-case ; sbcl raises an error if dispatch-char already a dispatch char
          (make-dispatch-macro-character dispatch-char t readtable)
        (error () nil))
      (set-dispatch-macro-character dispatch-char open-char
                                    #'hash-dispatch-reader readtable)
      (set-macro-character close-char #'read-close-delimiter nil readtable))))


;;;; make-hash.lisp ends here
