;;;; $Id: test-parse-message.lisp 184 2007-04-19 21:54:49Z ehuelsmann $
;;;; $URL: file:///project/cl-irc/svn/tags/0.9.2/test/test-parse-message.lisp $

;;;; See the LICENSE file for licensing information.

(in-package :cl-irc-test)

(defvar *msg1* (format nil ":kire!~~eenge@216.248.178.227 PRIVMSG cl-irc :heyhey!~A" #\Return))
(defvar *msg2* (format nil ":tolkien.freenode.net 372 cl-irc :-~A" #\Return))
(defvar *msg3* (format nil "NOTICE AUTH :*** Your forward and reverse DNS don't match~A" #\Return))
(defvar *msg4* (format nil ":kire_!~~eenge@adsl-156-35-240.asm.bellsouth.net MODE #lisppaste +k key~A" #\Return))
(defvar *msg5* (format nil ":kire_!~~eenge@adsl-156-35-240.asm.bellsouth.net MODE #lisppaste +bbb *!*@somewhere.com *!*@somewhereles.com *!*@youdontwannaknow.org~A" #\Return))
(defvar *msg6* (format nil ":kire!~~eenge@216.248.178.227 PRIVMSG cl-irc heyhey!~A" #\Return))
(defvar *msg7* (format nil ":ChanServ!ChanServ@services. MODE #lisppaste +o eh "))


(deftest find-reply-name.1 (irc:find-reply-name 1) :rpl_welcome)
(deftest find-reply-name.2
    (handler-bind ((irc:no-such-reply #'continue))
      (irc:find-reply-name 999)) :unknown-reply)

(deftest return-source.1 (irc::return-source cl-irc-test::*msg1*)  5 "kire")
(deftest return-source.2 (irc::return-source cl-irc-test::*msg2*) 21 "tolkien.freenode.net")
(deftest return-source.3 (irc::return-source cl-irc-test::*msg3*) 0 nil)
(deftest return-source.4 (irc::return-source cl-irc-test::*msg4*) 6 "kire_")
(deftest return-source.5 (irc::return-source cl-irc-test::*msg5*) 6 "kire_")

(deftest return-user.1 (irc::return-user cl-irc-test::*msg1* :start 5) 12 "~eenge")
(deftest return-user.2 (irc::return-user cl-irc-test::*msg2* :start 21) 21 nil)
(deftest return-user.3 (irc::return-user cl-irc-test::*msg3* :start 0) 0 nil)
(deftest return-user.4 (irc::return-user cl-irc-test::*msg4* :start 6) 13 "~eenge")
(deftest return-user.5 (irc::return-user cl-irc-test::*msg5* :start 6) 13 "~eenge")

(deftest return-host.1 (irc::return-host cl-irc-test::*msg1* :start 12) 28 "216.248.178.227")
(deftest return-host.2 (irc::return-host cl-irc-test::*msg2* :start 21) 21 nil)
(deftest return-host.3 (irc::return-host cl-irc-test::*msg3* :start 0) 0 nil)
(deftest return-host.4 (irc::return-host cl-irc-test::*msg4* :start 13) 47 "adsl-156-35-240.asm.bellsouth.net")
(deftest return-host.5 (irc::return-host cl-irc-test::*msg5* :start 13) 47 "adsl-156-35-240.asm.bellsouth.net")

(deftest return-command.1 (irc::return-command cl-irc-test::*msg1* :start 28) 36 "PRIVMSG")
(deftest return-command.2 (irc::return-command cl-irc-test::*msg2* :start 21) 25 "372")
(deftest return-command.3 (irc::return-command cl-irc-test::*msg3* :start 0) 6 "NOTICE")
(deftest return-command.4 (irc::return-command cl-irc-test::*msg4* :start 47) 52 "MODE")
(deftest return-command.5 (irc::return-command cl-irc-test::*msg5* :start 47) 52 "MODE")

(deftest return-arguments.1 (irc::return-arguments cl-irc-test::*msg1* :start 36) 44 ("cl-irc"))
(deftest return-arguments.2 (irc::return-arguments cl-irc-test::*msg2* :start 25) 33 ("cl-irc"))
(deftest return-arguments.3 (irc::return-arguments cl-irc-test::*msg3* :start 6) 12 ("AUTH"))
(deftest return-arguments.4 (irc::return-arguments cl-irc-test::*msg4* :start 52) 70 ("#lisppaste" "+k" "key"))
(deftest return-arguments.5 (irc::return-arguments cl-irc-test::*msg5* :start 52) 132 ("#lisppaste" "+bbb" "*!*@somewhere.com" "*!*@somewhereles.com" "*!*@youdontwannaknow.org"))

(deftest return-trailing-argument.1
    (irc::return-trailing-argument cl-irc-test::*msg1* :start 44) 52 "heyhey!")
(deftest return-trailing-argument.2
    (irc::return-trailing-argument cl-irc-test::*msg2* :start 33) 35 "-")
(deftest return-trailing-argument.3
    (irc::return-trailing-argument cl-irc-test::*msg3* :start 12) 57 "*** Your forward and reverse DNS don't match")
(deftest return-trailing-argument.4
    (irc::return-trailing-argument cl-irc-test::*msg4* :start 70) 70 nil)
(deftest return-trailing-argument.5
    (irc::return-trailing-argument cl-irc-test::*msg5* :start 132) 132 nil)

(deftest parse-raw-message.1
    (irc::parse-raw-message cl-irc-test::*msg1*)
  "kire" "~eenge" "216.248.178.227" "PRIVMSG" ("cl-irc" "heyhey!"))

(deftest no-trailing.1
  (irc::parse-raw-message *msg6*)
  "kire" "~eenge" "216.248.178.227" "PRIVMSG" ("cl-irc" "heyhey!"))

(deftest mode.1
  (irc::parse-raw-message *msg7*)
  "ChanServ" "ChanServ" "services." "MODE" ("#lisppaste" "+o" "eh"))
