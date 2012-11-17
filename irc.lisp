(defpackage #:lrssl
  (:use #:cl #:sys.net)
  (:export lrssl))

(in-package #:lrssl)

(defparameter *numeric-replies*
  '((401 :err-no-such-nick)
    (402 :err-no-such-server)
    (403 :err-no-such-channel)
    (404 :err-cannot-send-to-channel)
    (405 :err-too-many-channels)
    (406 :err-was-no-such-nick)
    (407 :err-too-many-targets)
    (409 :err-no-origin)
    (411 :err-no-recipient)
    (412 :err-no-text-to-send)
    (413 :err-no-toplevel)
    (414 :err-wild-toplevel)
    (421 :err-unknown-command)
    (422 :err-no-motd)
    (423 :err-no-admin-info)
    (424 :err-file-error)
    (431 :err-no-nickname-given)
    (432 :err-erroneus-nickname)
    (433 :err-nickname-in-use)
    (436 :err-nick-collision)
    (441 :err-user-not-in-channel)
    (442 :err-not-on-channel)
    (443 :err-user-on-channel)
    (444 :err-no-login)
    (445 :err-summon-disabled)
    (446 :err-users-disabled)
    (451 :err-not-registered)
    (461 :err-need-more-params)
    (462 :err-already-registred)
    (463 :err-no-perm-for-host)
    (464 :err-password-mismatch)
    (465 :err-youre-banned-creep)
    (467 :err-key-set)
    (471 :err-channel-is-full)
    (472 :err-unknown-mode)
    (473 :err-invite-only-chan)
    (474 :err-banned-from-chan)
    (475 :err-bad-channel-key)
    (481 :err-no-privileges)
    (482 :err-chanop-privs-needed)
    (483 :err-cant-kill-server)
    (491 :err-no-oper-host)
    (501 :err-umode-unknown-flag)
    (502 :err-users-dont-match)
    (300 :rpl-none)
    (302 :rpl-userhost)
    (303 :rpl-ison)
    (301 :rpl-away)
    (305 :rpl-unaway)
    (306 :rpl-nowaway)
    (311 :rpl-whoisuser)
    (312 :rpl-whoisserver)
    (313 :rpl-whoisoperator)
    (317 :rpl-whoisidle)
    (318 :rpl-endofwhois)
    (319 :rpl-whoischannels)
    (314 :rpl-whowasuser)
    (369 :rpl-endofwhowas)
    (321 :rpl-liststart)
    (322 :rpl-list)
    (323 :rpl-listend)
    (324 :rpl-channelmodeis)
    (331 :rpl-notopic)
    (332 :rpl-topic)
    (333 :rpl-topic-time)
    (341 :rpl-inviting)
    (342 :rpl-summoning)
    (351 :rpl-version)
    (352 :rpl-whoreply)
    (315 :rpl-endofwho)
    (353 :rpl-namreply)
    (366 :rpl-endofnames)
    (364 :rpl-links)
    (365 :rpl-endoflinks)
    (367 :rpl-banlist)
    (368 :rpl-endofbanlist)
    (371 :rpl-info)
    (374 :rpl-endofinfo)
    (375 :rpl-motdstart)
    (372 :rpl-motd)
    (376 :rpl-endofmotd)
    (381 :rpl-youreoper)
    (382 :rpl-rehashing)
    (391 :rpl-time)
    (392 :rpl-usersstart)
    (393 :rpl-users)
    (394 :rpl-endofusers)
    (395 :rpl-nousers)
    (200 :rpl-tracelink)
    (201 :rpl-traceconnecting)
    (202 :rpl-tracehandshake)
    (203 :rpl-traceunknown)
    (204 :rpl-traceoperator)
    (205 :rpl-traceuser)
    (206 :rpl-traceserver)
    (208 :rpl-tracenewtype)
    (261 :rpl-tracelog)
    (211 :rpl-statslinkinfo)
    (212 :rpl-statscommands)
    (213 :rpl-statscline)
    (214 :rpl-statsnline)
    (215 :rpl-statsiline)
    (216 :rpl-statskline)
    (218 :rpl-statsyline)
    (219 :rpl-endofstats)
    (241 :rpl-statslline)
    (242 :rpl-statsuptime)
    (243 :rpl-statsoline)
    (244 :rpl-statshline)
    (221 :rpl-umodeis)
    (251 :rpl-luserclient)
    (252 :rpl-luserop)
    (253 :rpl-luserunknown)
    (254 :rpl-luserchannels)
    (255 :rpl-luserme)
    (256 :rpl-adminme)
    (257 :rpl-adminloc1)
    (258 :rpl-adminloc2)
    (259 :rpl-adminemail)))

(defun decode-command (line)
  "Explode a line into (prefix command parameters...)."
  ;; <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
  ;; <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
  ;; <command>  ::= <letter> { <letter> } | <number> <number> <number>
  ;; <SPACE>    ::= ' ' { ' ' }
  ;; <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
  (let ((prefix nil)
        (offset 0)
        (command nil)
        (parameters '()))
    (when (and (not (zerop (length line)))
               (eql (char line 0) #\:))
      ;; Prefix present, read up to a space.
      (do () ((or (>= offset (length line))
                  (eql (char line offset) #\Space)))
        (incf offset))
      (setf prefix (subseq line 1 offset)))
    ;; Eat leading spaces.
    (do () ((or (>= offset (length line))
                (not (eql (char line offset) #\Space))))
      (incf offset))
    ;; Parse a command, reading until space or the end.
    (let ((start offset))
      (do () ((or (>= offset (length line))
                  (eql (char line offset) #\Space)))
        (incf offset))
      (setf command (subseq line start offset)))
    (when (and (= (length command) 3)
               (every (lambda (x) (find x "1234567890")) command))
      (setf command (parse-integer command))
      (setf command (or (second (assoc command *numeric-replies*))
                        command)))
    ;; Read parameters.
    (loop
       ;; Eat leading spaces.
       (do () ((or (>= offset (length line))
                   (not (eql (char line offset) #\Space))))
         (incf offset))
       (cond ((>= offset (length line)) (return))
             ((eql (char line offset) #\:)
              (push (subseq line (1+ offset)) parameters)
              (return))
             (t (let ((start offset))
                  (do () ((or (>= offset (length line))
                              (eql (char line offset) #\Space)))
                    (incf offset))
                  (push (subseq line start offset) parameters)))))
    (values prefix command (nreverse parameters))))

(defun parse-command (line)
  (let ((command-end nil)
        (rest-start nil)
        (rest-end nil))
    (dotimes (i (length line))
      (when (eql (char line i) #\Space)
        (setf command-end i
              rest-start i)
        (return)))
    (when rest-start
      ;; Eat leading spaces.
      (do () ((or (>= rest-start (length line))
                  (not (eql (char line rest-start) #\Space))))
        (incf rest-start)))
    (values (subseq line 1 command-end)
            (subseq line (or rest-start (length line)) rest-end))))

(defun send (stream control-string &rest arguments)
  "Buffered FORMAT."
  (declare (dynamic-extent argument))
  (write-sequence (apply 'format nil control-string arguments) stream))

(defvar *command-table* (make-hash-table :test 'equal))

(defmacro define-server-command (name lambda-list &body body)
  (let ((args (gensym)))
    `(setf (gethash ,(if (and (symbolp name) (not (keywordp name)))
                         (symbol-name name)
                         name)
                    *command-table*)
           (lambda (,(first lambda-list) ,args)
             (declare (system:lambda-name (irc-command ,name)))
             (destructuring-bind ,(rest lambda-list) ,args
               ,@body)))))

(define-server-command privmsg (from channel message)
  (format t "[~A]<~A> ~A~%" channel from message))

(defun lrssl-rx (connection display)
  (with-simple-restart (abort "Give up")
    (loop (let ((line (read-line connection)))
            (multiple-value-bind (prefix command parameters)
                (decode-command line)
              (let ((fn (gethash command *command-table*)))
                (cond (fn (let ((*standard-output* display))
                            (funcall fn prefix parameters)))
                      ((keywordp command)
                       (format display "[~A] -!- ~A~%" prefix (car (last parameters))))
                      ((integerp command)
                       (format display "[~A] ~D ~A~%" prefix command parameters))
                      (t (write-line line display)))))))))

(defun lrssl (nick &optional (server '(213 92 8 4)) (port 6667))
  (sys.int::with-saved-screen (main-fb)
    (let* ((dims (array-dimensions main-fb))
           (display-fb (make-array (list (- (first dims) 20) (second dims))
                                   :displaced-to main-fb
                                   :displaced-index-offset 0))
           (display (make-instance 'sys.int::framebuffer-stream
                                   :framebuffer display-fb))
           (input-fb (make-array (list 16 (second dims))
                                 :displaced-to main-fb
                                 :displaced-index-offset (* (- (first dims) 16) (second dims))))
           (input (make-instance 'sys.int::framebuffer-stream
                                 :framebuffer input-fb)))
      (sys.int::%bitset (first dims) (second dims) 0 main-fb 0 0)
      (sys.int::%bitset 2 (second dims) #xFFD8D8D8 main-fb (- (first dims) 20) 0)
      (with-open-stream (connection (sys.net::tcp-stream-connect server port))
        (sys.int::with-process ("LRSSL receiver" #'lrssl-rx connection display)
          (let ((current-channel nil)
                (joined-channels '()))
            (send connection "USER ~A hostname servername :~A~%" nick nick)
            (send connection "NICK ~A~%" nick)
            (loop (format input "~A] " (or current-channel ""))
               (let ((line (read-line input)))
                 (sys.int::stream-move-to input 0 0)
                 (cond ((and (>= (length line) 1)
                             (eql (char line 0) #\/)
                             (not (and (>= (length line) 2)
                                       (eql (char line 1) #\/))))
                        (multiple-value-bind (command rest)
                            (parse-command line)
                          (cond ((string-equal command "quit")
                                 (send connection "QUIT :~A~%" rest)
                                 (return))
                                ((string-equal command "join")
                                 (send connection "JOIN ~A~%" rest)
                                 (push rest joined-channels)
                                 (unless current-channel
                                   (setf current-channel rest)))
                                ((string-equal command "chan")
                                 (setf current-channel rest))
                                ((string-equal command "part")
                                 (when current-channel
                                   (send connection "PART ~A :~A~%" current-channel rest)
                                   (setf current-channel nil)))
                                ((string-equal command "raw")
                                 (write-string rest connection)
                                 (terpri connection))
                                ((string-equal command "eval")
                                 (format display "[eval] ~A~%" rest)
                                 (let ((*standard-output* display)
                                       (*query-io* input)
                                       (*standard-input* input))
                                   (with-simple-restart (abort "Abort evaulation and return to LRSSL.")
                                     (eval (read-from-string rest)))
                                   (fresh-line display)))
                                ((string-equal command "say")
                                 (when current-channel
                                   (format display "[~A]<~A> ~A~%" current-channel nick rest)
                                   (send connection "PRIVMSG ~A :~A~%" current-channel rest)))
                                (t (format display "Unknown command ~S.~%" command)))))
                       (current-channel
                        (format display "[~A]<~A> ~A~%" current-channel nick line)
                        (send connection "PRIVMSG ~A :~A~%" current-channel line)))))))))))
