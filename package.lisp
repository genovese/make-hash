;;;; package.lisp

(defpackage #:make-hash
  (:use #:cl)
  (:export #:make-hash
           #:make-hash-transformer
           #:initialize-hash
           #:hash-initializer-default-format
           #:*hash-factory-defaults*
           #:define-hash-factory
           #:make-hash-factory
           #:install-hash-reader
           #:make-hash-error))


