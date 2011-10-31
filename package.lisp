;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :rcw-mexp
  (:use)
  (:export :macroexpand-all))

(defpackage :rcw-mexp-internal
  (:use :rcw-mexp :cl :fiveam))

