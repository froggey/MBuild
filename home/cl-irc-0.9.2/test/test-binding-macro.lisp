;;;; $Id: test-binding-macro.lisp 161 2006-05-23 20:40:48Z ehuelsmann $
;;;; $URL: file:///project/cl-irc/svn/tags/0.9.2/test/test-binding-macro.lisp $

;;;; See the LICENSE file for licensing information.


(in-package :cl-irc-test)

(defvar *protocol-mode*
  ":Chanserv!chanserve@services. MODE #svn +o eh")


;; tests which should complete successfully

(deftest binding.1
  (destructuring-arguments
       (target modes &rest arguments)
       (cl-irc::create-irc-message *protocol-mode*)
     (values target modes arguments))
  "#svn" "+o" ("eh"))


(deftest binding.2
  (destructuring-arguments
       (target :ignored &rest arguments)
       (cl-irc::create-irc-message *protocol-mode*)
     (values target arguments))
  "#svn" ("eh"))

(deftest binding.3
  (destructuring-arguments
       (:ignored &rest arguments &req nick)
       (cl-irc::create-irc-message *protocol-mode*)
     (values nick arguments))
  "eh" ("+o"))

(deftest binding.4
  (destructuring-arguments
       (target &optional modes &req nick)
       (cl-irc::create-irc-message *protocol-mode*)
     (values target modes nick))
  "#svn" "+o" "eh")

(deftest binding.5
  (destructuring-arguments
       (&whole all target &optional modes &req nick)
       (cl-irc::create-irc-message *protocol-mode*)
     (values all target modes nick))
  ("#svn" "+o" "eh") "#svn" "+o" "eh")

(deftest binding.6
  (destructuring-arguments
       (target &optional modes &rest args &req nick)
       (cl-irc::create-irc-message *protocol-mode*)
     (values target modes args nick))
  "#svn" "+o" nil "eh")
