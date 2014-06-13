#|
  This file is a part of networks-search project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage networks-search-test-asd
  (:use :cl :asdf))
(in-package :networks-search-test-asd)

(defsystem networks-search-test
  :author "K. Isom"
  :license "ISC"
  :depends-on (:networks-search
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "networks-search")))))
