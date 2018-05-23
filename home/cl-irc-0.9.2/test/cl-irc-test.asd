;;;; $Id: cl-irc-test.asd 161 2006-05-23 20:40:48Z ehuelsmann $
;;;; $URL: file:///project/cl-irc/svn/tags/0.9.2/test/cl-irc-test.asd $

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-irc-test-system
    (:use #:cl #:asdf))

(in-package #:cl-irc-test-system)

(defsystem cl-irc-test
    :name "cl-irc-test"
    :author "Erik Enge"
    :version "0.1.0"
    :licence "MIT"
    :description "Tests for the cl-irc system"
    :depends-on (:split-sequence :rt :cl-irc)
    :components ((:file "package")
                 (:file "test-parse-message"
                        :depends-on ("package"))
                 (:file "test-protocol"
                        :depends-on ("test-parse-message"))
                 (:file "test-binding-macro"
                        :depends-on ("package"))))
