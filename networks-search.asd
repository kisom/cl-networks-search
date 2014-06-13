#|
  This file is a part of networks-search project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

#|
  Author: K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage networks-search-asd
  (:use :cl :asdf))
(in-package :networks-search-asd)

(defsystem networks-search
  :version "0.1"
  :author "K. Isom"
  :license "ISC"
  :depends-on (:networks-graph)
  :components ((:module "src"
                :components
                ((:file "networks-search"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op networks-search-test))))
