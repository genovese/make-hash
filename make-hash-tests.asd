(asdf:defsystem #:make-hash-tests
  :depends-on (:make-hash :fiveam)
  :components ((:file "tests")))

(in-package :asdf)
(defmethod perform ((o test-op) (c (eql (find-system "make-hash-tests"))))
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:run-tests) '#:make-hash-tests) args)))
    (run-tests)))
(in-package :cl-user)

