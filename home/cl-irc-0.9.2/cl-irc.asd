;;;; $Id: cl-irc.asd 245 2015-09-06 13:16:49Z ehuelsmann $
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-irc-system
    (:use #:cl #:asdf))

(in-package #:cl-irc-system)

(defsystem cl-irc
    :name "cl-irc"
    :author "Erik Enge & Contributors"
    :version "0.9.2"
    :licence "MIT"
    :description "Common Lisp interface to the IRC protocol"
    :depends-on (:split-sequence :usocket :flexi-streams)
    :properties ((#:author-email . "cl-irc-devel@common-lisp.net")
                 (#:date . "$Date: 2015-09-06 13:16:49 +0000 (Sun, 06 Sep 2015) $")
                 ((#:albert #:output-dir) . "doc/api-doc/")
                 ((#:albert #:formats) . ("docbook"))
                 ((#:albert #:docbook #:template) . "book")
                 ((#:albert #:docbook #:bgcolor) . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :components ((:file "package")
                 (:file "variable"
                        :depends-on ("package"))
                 (:file "utility"
                        :depends-on ("variable"))
                 (:file "parse-message"
                        :depends-on ("utility"))
                 (:file "protocol"
                        :depends-on ("parse-message"))
                 (:file "command"
                        :depends-on ("protocol"))
                 (:file "event"
                        :depends-on ("command"))))
