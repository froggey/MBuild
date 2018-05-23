;;;; $Id: package.lisp 161 2006-05-23 20:40:48Z ehuelsmann $
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :cl-irc-test
      (:use :cl :rt :cl-irc)
    (:nicknames :cl-irc-test)
    (:export :do-tests)))
