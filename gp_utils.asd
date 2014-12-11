(defpackage :gp-utils-asd
  (:use :cl :asdf))

(in-package :gp-utils-asd)

(defsystem :gp_utils
  :name "gp_utils"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Graphics Port Utilities for LispWorks."
  :serial t
  :components ((:file "gp_utils")))
