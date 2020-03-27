(in-package :cl-user)

(defpackage #:nal
  (:nicknames #:tbnl-test)
  (:use :cl :cl-who :hunchentoot :parenscript :parseq :cacle)
  (:export #:nal))
  
(defpackage #:nal-user
  (:use :cl :hunchentoot))