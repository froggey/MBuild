;;;; $Id: cliki.lisp 210 2008-06-30 20:52:34Z ehuelsmann $
;;;; $Source$

;;;; cliki.lisp - CLiki as an infobot; only works on SBCL.

;;; To use it, load the net-nittin-irc and cl-ppcre systems, load
;;; cliki.lisp, and invoke (cliki::start-cliki-bot "desirednickname"
;;; "desiredserver" "#channel1" "#channel2" "#channel3" ...)

(defpackage :cliki (:use :common-lisp :irc :cl-ppcre :split-sequence)
  (:export :start-cliki-bot :*cliki-nickserv-password*
	   :*respond-to-general-hellos* :shut-up :un-shut-up))
(in-package :cliki)

(defvar *small-definitions* nil)

(defvar *aliases* nil)

(defparameter *sd-file*
  (merge-pathnames "sd.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     (or *load-truename*
                         *default-pathname-defaults*)))))

(defun forget (term-or-alias)
  (setf *small-definitions* (remove term-or-alias *small-definitions* :test #'string-equal :key #'car))
  (setf *aliases* (remove term-or-alias *aliases* :test #'string-equal :key #'car))
  (write-small-definitions))

(defun fix-aliases ()
  (setf *small-definitions*
        (loop for defn in *small-definitions*
              if (stringp (cdr defn))
              collect defn
              else do (push (cons (first defn) (second defn))
                            *aliases*))))

(defun read-small-definitions ()
  (setf *small-definitions* nil)
  (setf *aliases* nil)
  (with-open-file (sd-file *sd-file* :direction :input :if-does-not-exist nil)
    (when sd-file
      (loop for defn = (read sd-file nil)
            if defn do (ecase (car defn)
                         (:sd (push (cdr defn) *small-definitions*))
                         (:alias (push (cdr defn) *aliases*)))
            else return *small-definitions*))))

(defun write-small-definitions ()
  (with-open-file (sd-file *sd-file* :direction :output :if-exists :supersede)
    (mapc #'(lambda (db)
              (mapc #'(lambda (defn)
                        (prin1 (cons (car db) defn) sd-file)
                        (format sd-file "~%")) (reverse (cdr db))))
          (list (cons :sd *small-definitions*)
                (cons :alias *aliases*)))))

(defun write-top-definition (&key (of *small-definitions*) (type :sd))
  (with-open-file (sd-file *sd-file* :direction :output :if-exists :append)
    (prin1 (cons type (car of)) sd-file)
    (format sd-file "~%")))

(defun add-small-definition (term defn)
  (push (cons term defn) *small-definitions*)
  (write-top-definition))

(defun add-alias (term defn)
  (push (cons term defn) *aliases*)
  (write-top-definition :of *aliases* :type :alias))

(defun cliki-apropos (search-string)
  (let ((found (remove-duplicates
                 (loop for (term . defn) in *small-definitions*
                      if (search search-string defn :test #'string-equal)
                      collect term) :test #'string-equal)))
    (if found (format nil "Matches: ~{~S~^ ~}" found)
        "No Matches")))

(defvar *lookup-depth* 0)

(defvar *followed-aliases* nil)

(defvar *last-lookup* "")
(defvar *last-lookup-source* "")
(defvar *last-lookup-time* (get-universal-time))

(defun alias-string-equal (orig candidate)
  (unless (member candidate *followed-aliases* :test #'string-equal)
    (string-equal orig candidate)))

(defun should-do-lookup (text source)
  (not (and (string-equal text *last-lookup*)
            (string-equal source *last-lookup-source*)
            (< (- (get-universal-time)
                  *last-lookup-time*) 5))))

(defun did-lookup (text source)
  (setf *last-lookup* text)
  (setf *last-lookup-source* source)
  (setf *last-lookup-time* (get-universal-time)))

(defmacro aif (test conseq &optional (else nil))
  `(let ((it ,test))
     (if it ,conseq
       (symbol-macrolet ((it ,test))
           ,else))))

(defun small-definition-lookup (text)
  (cdr (assoc text *small-definitions* :test #'string-equal)))

(defun alias-lookup (text)
  (let ((alias (or (cdr (assoc text *aliases* :test #'alias-string-equal))
                   (car (rassoc text *aliases* :test #'alias-string-equal)))))
    (if alias
        (let ((*lookup-depth* (1+ *lookup-depth*))
              (*followed-aliases* (cons alias *followed-aliases*)))
          (if (> *lookup-depth* 5)
              "Too many recursive lookups."
              (cliki-lookup alias))))))

(defclass memo ()
  ((from :accessor memo-from :initarg :from)
   (to :accessor memo-to :initarg :to)
   (contents :accessor memo-contents :initarg :contents)))

(defun without-non-alphanumeric (string)
  (with-output-to-string (s)
    (loop for char across string
          if (alphanumericp char)
          do (princ char s))))

(defvar *pending-memos* nil)

(defun memo-alias-test (orig candidate)
  (or (string-equal orig (car candidate))
      (string-equal orig (cdr candidate))
      (string-equal orig (without-non-alphanumeric (car candidate)))
      (string-equal orig (without-non-alphanumeric (cdr candidate)))))

(defun take-care-of-memos (channel user &key (original-user user) (no-alias nil))
  (let ((found (find (without-non-alphanumeric user) *pending-memos* :test #'string-equal :key #'memo-to :from-end t)))
    (if found
        (progn
          (setf *pending-memos* (remove found *pending-memos*))
          (privmsg *cliki-connection* channel (format nil "~A, memo from ~A: ~A" original-user (memo-from found) (memo-contents found)))
          (take-care-of-memos channel user :original-user original-user))
        (if (not no-alias)
            (let ((alias (find (without-non-alphanumeric user)
                               *aliases*
                               :test #'memo-alias-test)))
              (if alias
                  (take-care-of-memos channel (cdr alias) :original-user original-user :no-alias t)))))))
  
(defun add-memo (from to contents)
  (push (make-instance 'memo :from from
                       :to (without-non-alphanumeric to)
                       :contents contents)
        *pending-memos*))

(defun remove-memos (to &key from)
  (let ((count 0))
    (setf *pending-memos*
          (remove-if #'(lambda (m)
                         (and (string-equal (without-non-alphanumeric to)
                                            (memo-to m))
                              (or (not from)
                                  (string-equal (without-non-alphanumeric from)
                                                (memo-from m)))
                              (incf count)))
                     *pending-memos*))
    count))

(defparameter *advice-file*
  (merge-pathnames "advice"
                   (make-pathname
                    :directory
                    (pathname-directory
                     (or *load-truename*
                         *default-pathname-defaults*)))))

(defvar *advice-db* nil)

(defun advice-db ()
  (when (not *advice-db*)
    (with-open-file (ad *advice-file* :direction :input)
      (setf *advice-db* (read ad))))
  *advice-db*)

(defun lookup-advice (num-str)
  (let ((num (parse-integer num-str :junk-allowed t)))
    (or (cdr (assoc num (advice-db)))
        "You can't just make up advice numbers and expect a response.")))

(defun random-advice ()
  (let ((item (random-element (advice-db))))
    (format nil "#~A: ~A" (car item) (cdr item))))

(defun search-advice (str)
  (setf str (regex-replace-all "\\s+" str " "))
  (setf str (regex-replace-all "[^a-zA-Z0-9 ]" str ""))
  (let* ((terms (split-sequence #\space str))
         (terms (mapcar #'(lambda (e)
                            (loop for r = (regex-replace-all "^(.+)(ness|ing|ation|ion|ly)$" e "\\1")
                                  if (equal e r) return r
                                  do (setf e r)
                                  )) terms))
         (terms (mapcar #'(lambda (e)
                            (regex-replace-all "^(.+)([a-zA-Z])\\2+$" e "\\1\\2")) terms))
         (terms (mapcar #'(lambda (e)
                            (regex-replace-all "^(.+)s$" e "\\1")) terms))
         (max-score 0)
         (max-score-items nil))
    ;;(format t "terms is ~S~%" terms)
    (mapc #'(lambda (e)
              (let ((score
                     (loop for i in terms
                           if (search i (cdr e) :test #'char-equal)
                           count it)))
                (if (> score max-score)
                    (progn
                      (setf max-score score)
                      (setf max-score-items (list e)))
                    (if (and (not (zerop score))
                             (eql score max-score))
                        (push e max-score-items)))))
          (advice-db))
    (if (zerop max-score)
        (progn
	  (signal 'lookup-failure)
	  "You can't expect automated advice for everything.")
        (let ((item (random-element max-score-items)))
          (format nil "#~A: ~A" (car item) (cdr item))))))

(defun lookup-paste (number)
  (and (find-package :lisppaste)
       (let ((paste (funcall (intern "FIND-PASTE" :lisppaste) number)))
         (and paste
              (format nil "Paste number ~A: \"~A\" by ~A in ~A. ~A"
                      number
                      (funcall (intern "PASTE-TITLE" :lisppaste) paste)
                      (funcall (intern "PASTE-USER" :lisppaste) paste)
                      (funcall (intern "PASTE-CHANNEL" :lisppaste) paste)
                      (funcall (intern "PASTE-DISPLAY-URL" :lisppaste) paste))))))

(defun encode-for-url (str)
  (setf str (regex-replace-all " " str "%20"))
  (setf str (regex-replace-all "," str "%2C"))
  (setf str (regex-replace-all "`" str "%60"))
  ;(format t "hi ~A~%" str)
  str)

#-(or sbcl ccl)
(defmacro host-with-timeout (timeout &body body)
  (declare (ignore timeout))
  `(progn ,@body))

#+sbcl
(defmacro host-with-timeout (timeout &body body)
  `(sb-ext:with-timeout ,timeout ,@body))

#+ccl
(defmacro host-with-timeout (timeout &body body)
  `(let ((interrupt-thread nil))
    (setf interrupt-thread
     (ccl:process-run-function 'timeout
      (let ((process ccl:*current-process*))
        (lambda ()
          (sleep ,timeout)
          (ccl:process-interrupt process
                                 (lambda ()
                                   (signal 'openmcl-timeout)))))))
    (unwind-protect
         (progn ,@body)
      (if interrupt-thread
          (ccl:process-kill interrupt-thread)))))

(defun http-get-recursively (url)
  (destructuring-bind (status headers stream)
      (trivial-http:http-get url)
    (if (and (eql status 302)
	     (assoc :location headers))
	(progn
	  (close stream)
	  (http-get-recursively (cdr (assoc :location headers))))
	(list status headers stream))))

(define-condition lookup-failure (condition) ())

(defun cliki-first-sentence (term)
  (let* ((cliki-url (format nil "http://www.cliki.net/~A"
			     (encode-for-url term)))
	  (url (concatenate 'string cliki-url "?source")))
     (block cliki-return
       (handler-case
	   (host-with-timeout
	    5
	    (destructuring-bind (status headers stream)
		(http-get-recursively url)
	     (declare (ignore headers))
	     ;; Please don't hack on this when tired; it's easy to make it leak fds.
	       (unwind-protect
		 (if (or (not (eql status 200)) (not stream))
			nil
			;;(format nil "The term ~A was not found in CLiki." term)
			(let ((first-line ""))
			  (loop for i from 1 to 5 do ;; scan the first 5 lines
			       (progn
				 (multiple-value-bind (next-line missing-newline-p)
				     (read-line stream nil)
				   (if next-line
				       (setf first-line (concatenate 'string first-line (string #\newline) next-line))
				       (return-from cliki-return (format nil "The end of the page was reached before a definition was found in ~A" cliki-url))))
				 (setf first-line (regex-replace-all "\\r" first-line " "))
				 (setf first-line (regex-replace-all "\\n" first-line " "))
				 (setf first-line (regex-replace-all "_\\(([^)]*)\\)" first-line "\\1"))
				 (setf first-line (regex-replace-all "#H\\(([^)]*)\\)" first-line "\\1"))
				 (setf first-line (regex-replace-all "\\*\\(([^)]*)\\)" first-line "\\1"))
				 (setf first-line (regex-replace-all "<[^>]+>" first-line ""))
				 (setf first-line (regex-replace-all "^(([^.]|\\.\\S)+)\\.\\s+.*$" first-line "\\1."))
				 (setf first-line (regex-replace-all "(\\s)\\s+" first-line "\\1"))
				 (setf first-line (regex-replace-all "^\\s*(.+\\S)\\s*$" first-line "\\1"))
				 (when (scan "^([^.]|\\.\\S)+[.?!]$" first-line)
				   (setf first-line (concatenate 'string first-line " " cliki-url))
				   (return-from cliki-return first-line))))
			  (progn
			    (signal 'lookup-failure)
			    (format nil "No definition was found in the first 5 lines of ~A" cliki-url))))
		 (if stream (close stream)))))
	 #+sbcl
	 (sb-ext:timeout (c)
	   (return-from cliki-return (progn (signal 'lookup-failure)
					    "I can't be expected to work when CLiki doesn't respond to me, can I?")))
	 (trivial-sockets:socket-error (c)
	   (return-from cliki-return (progn (signal 'lookup-failure)
					    "I can't be expected to work when CLiki doesn't respond to me, can I?")))
	 (serious-condition (c &rest whatever) (return-from cliki-return (progn (signal 'lookup-failure) (regex-replace-all "\\n" (format nil "An error was encountered in lookup: ~A." c) " "))))))
     ))

(defun shorten (url)
  (handler-case
      (let ((stream (trivial-http:http-get (format nil "http://shorl.com/create.php?url=~A" url))))
        (finish-output t)
        (unwind-protect
             (when stream
               (prog1
                   (loop for line = (read-line stream nil nil)
                         while line
                         if (scan "http://shorl\\.com/[a-z]+" line)
                         return (regex-replace-all "^.*(http://shorl\\.com/[a-z]+).*$" line "\\1"))
                 (close stream)
                 (setf stream nil)))
          (if stream (close stream))))
    (condition (c)
      (return-from shorten (regex-replace-all "\\n" (format nil "An error was encountered in shorten: ~A." c) " ")))))

(defvar *cliki-connection*)
(defvar *cliki-nickname*)

(defun shut-up ()
  (setf (irc:client-stream *cliki-connection*) (make-broadcast-stream)))

(defun un-shut-up ()
  (setf (irc:client-stream *cliki-connection*) *trace-output*))



(defun make-cliki-attention-prefix (nick)
  (format nil "^(?i)~A[,:]\\s+" nick))

(defvar *cliki-attention-prefix* "")

(defparameter *help-text*
  `(("lookups" . ,(lambda (nick)
                          (format nil "To look up a term, say something like ``~A: term?''. I will either return a definition for the term or say that it could not be found. Lookups check the internal database first and then try to retrieve the first sentence of the page named like that on CLiki." nick)))
    ("helping others" .
     ,(lambda (nick)
              (format nil "I can tell another user about something if you address me like ``~A: show some-user something else''. I respond to a lot of different ways of asking for this, and you can have me show pretty much anything to another user." nick)))
    ("adding terms" .
     ,(lambda (nick)
              (format nil "To add a term, say something like ``~A: add \"term\" as: the definition''. I will remember the definition." nick)))
    ("aliasing terms" .
     ,(lambda (nick)
              (format nil "To make a term an alias for another term, say something like ``~A: alias \"term\" as: some other term''. I will remember the alias." nick)))
    ("forgetting" .
     ,(lambda (nick)
              (format nil "To make me forget something, say something like ``~A: forget term''. I'll forget what I know about that term or nickname." nick)))
    ("memos" .
     ,(lambda (nick)
              (format nil "To send a memo, say something like ``~A: memo for nick: the memo''. I'll remember the memo for any nick which is the same as the given nick, +/- differences in punctuation, and any nick which is an alias for it, and give it to them when they next speak." nick)))
    ("avoiding memos" .
     ,(lambda (nick)
              (format nil "To flush all your memos without delivery, say something like ``~A: discard my memos''. To flush only memos from a specific person, say ``~A: discard my memos from person''." nick nick)))
    ("nicknames" .
     ,(lambda (nick)
              (format nil "If you have multiple nicknames and want to get your memos at any of them, say something like ``~A: nick1 is another nick for nick2''. If you decide to give up a nick, say ``~:*~A: forget nick2'' and I'll forget it." nick)))
    ("goodies" .
     ,(lambda (nick)
              (format nil "If I'm connected to a lisppaste bot, try ``~A: paste 42'' or some other number." nick)))
    ("eliza" .
     ,(lambda (nick)
              (declare (ignore nick))
              (format nil "If you say multiple words to me which I don't recognize and it's not found as a lookup, you might get a sarcastic reply. Don't abuse this too much.")))
    ("advice" .
     ,(lambda (nick)
              (format nil "Try saying something like ``~A: advice #11904'' to get some advice." nick)))
    ("apropos" .
     ,(lambda (nick)
              (format nil "Try ``~A: apropos foo'' to search for all small definitions containing ''foo''." nick)))
    ("acronyms" .
     ,(lambda (nick)
	(format nil "See an acronym you don't recognize? Try ``~A: what does sbcl stand for?'' to find out what it means!" nick)))))

(defun cliki-bot-help (nick)
  (format nil "There are multiple help modules. Try ``/msg ~A help kind'', where kind is one of: ~{\"~A\"~^, ~}."
          nick
          (mapcar #'car *help-text*)))

(defun cliki-find-help (string)
  (and (> (length string) 0)
       (let ((resp-generator (cdr (assoc string *help-text* :test #'string-equal))))
         (if resp-generator
             (funcall resp-generator *cliki-nickname*)
             (if (not (char-equal (elt string (1- (length string))) #\s))
                 (cliki-find-help (concatenate 'string string
                                               (string #\s))))))))

(defun random-element (list)
  (elt list (random (length list))))

(defparameter *last-eliza-times* (make-list 6 :initial-element 0))

(defparameter *last-warning-time* 0)

(defmacro without-abuse (&body body)
  `(flet ((doit () ,@body))
     (if (> (- (get-universal-time) 60)
	    *last-warning-time*)
	 (let ((time-6 (first *last-eliza-times*))
	       (time-4 (third *last-eliza-times*))
	       (time-2 (fifth *last-eliza-times*))
	       (current-time (get-universal-time))
	       (count 0)
	       (overload 0))
	   (if (or
		(and
		 (< (- current-time 60)
		    time-2)
		 (setf count 3)
		 (setf overload (- current-time time-2)))
		(and
		 (< (- current-time 75)
		    time-4)
		 (setf count 5)
		 (setf overload (- current-time time-4)))
		(and
		 (< (- current-time 90)
		    time-6)
		 (setf count 7)
		 (setf overload (- current-time time-6))))
	       (progn
		 (setf *last-warning-time* (get-universal-time))
		 (format nil "Would you /please/ stop playing with me? ~A messages in ~A seconds is too many." count overload))
	       (progn
		 (setf *last-eliza-times* (nconc (cdr *last-eliza-times*)
						 (list (get-universal-time))))
		 (doit))
	       
	     )))))

(defun do-eliza (first-pass)
  (without-abuse (ignore-errors (eliza::eliza first-pass))))

(defvar *more* "CODE")

(defvar *prepositions*
  '("aboard"  "about"  "above"  "across"  "after"  "against"  "along"  "among"  "around"  "as"   "at"  "before"  "behind"   "below" "beneath" "beside"  "between"  "beyond"  "but" "except"  "by"  "concerning"  "despite"  "down"  "during"  "except" "for"  "from"  "in"  "into"  "like" "near"  "of"  "off"  "on"  "onto"  "out"  "outside"  "over"  "past"  "per"  "regarding"  "since"  "through" "throughout"  "till"  "to"  "toward"  "under" "underneath"  "until"  "up"   "upon"  "with"  "within"  "without"))

(defvar *conjunctions*
  '("for" "and" "nor" "but" "or" "yet" "so"))

(defvar *articles*
  '("an" "a" "the"))

(defun scan-for-more (s)
  (let ((str (nth-value 1 (scan-to-strings "MORE\\W+((\\W|[A-Z0-9])+)([A-Z0-9])($|[^A-Z0-9])" s))))
    (or
     (and str
          (setf *more* (concatenate 'string (elt str 0) (elt str 2))))
     (let ((str (nth-value 1 (scan-to-strings "(?i)more\\W+(\\w+)\\W+(\\w+)\\W+(\\w+)" s))))
       (or
        (and str
             (or (member (elt str 0) *prepositions* :test #'string-equal)
                 (member (elt str 0) *conjunctions* :test #'string-equal)
                 (member (elt str 0) *articles* :test #'string-equal))
             (or (member (elt str 1) *prepositions* :test #'string-equal)
                 (member (elt str 1) *conjunctions* :test #'string-equal)
                 (member (elt str 1) *articles* :test #'string-equal))
             (setf *more* (string-upcase
                           (concatenate 'string (elt str 0) " " (elt str 1)
                                        " " (elt str 2)))))
        (let ((str (nth-value 1 (scan-to-strings "(?i)more\\W+(\\w+)\\W+(\\w+)" s))))
          (or
           (and str
                (or (member (elt str 0) *prepositions* :test #'string-equal)
                    (member (elt str 0) *conjunctions* :test #'string-equal)
                    (member (elt str 0) *articles* :test #'string-equal))
                (setf *more* (string-upcase
                              (concatenate 'string (elt str 0) " " (elt str 1)))))
           (let ((str (nth-value 1 (scan-to-strings "(?i)more\\W+(\\w+)" s))))
             (or
              (and str (setf *more* (string-upcase (elt str 0))))
              )))))))))
    
(defun cliki-lookup (term-with-question &key sender channel)
  (let ((first-pass (regex-replace-all "^(\\s*)(.*[^?.!,;])([?.!,;]*)$" term-with-question "\\2"))
        (should-send-cant-find t))
    (setf first-pass (regex-replace-all "\\s\\s+" first-pass " "))
    (setf first-pass (regex-replace-all "\\s*$" first-pass ""))
    (let ((scanned (or (nth-value 1 (scan-to-strings "^add\\s+\"(.+)\"\\s+as:*\\s+(.+)$" first-pass))
                       (nth-value 1 (scan-to-strings "^add\\s+(.+)\\s+as:*\\s+(.+)$" first-pass)))))
      (if scanned
          (let ((term (elt scanned 0))
                (defn (elt scanned 1)))
            (add-small-definition term defn)
            "OK, done.")
          (let ((scanned (or
                          (nth-value 1 (scan-to-strings "^alias\\s+\"([^\"]+)\"\\s+as:*\\s+(.+)$" first-pass))
                          (nth-value 1 (scan-to-strings "^alias\\s+(.+)\\s+as:*\\s+(.+)$" first-pass))
                          (nth-value 1 (scan-to-strings "^(.+)\\s+is\\s+another\\s+(name|word)\\s+for:*\\s+([^.]+)\\.*$" first-pass)))))
            (if scanned
                (let ((term (elt scanned 0))
                      (defn (elt scanned (1- (length scanned)))))
                  (add-alias term defn)
                  "OK, done.")
                (progn
                  (setf first-pass (regex-replace-all "(:|/|\\\\|\\#)" first-pass ""))
                  (setf first-pass (regex-replace-all "^(?i)(.*[^, ])(,|)\\s*please$" first-pass "\\1"))
                  (setf first-pass (regex-replace-all "^(?i)please(,|)\\s*(.*[^, ])$" first-pass "\\2"))
                  (when (and (scan "^(?i)lisppaste(\\s|!|\\?|\\.|$)*" first-pass)
                             (find-package :lisppaste)
                             channel
                             (> (length channel) 0)
                             (char= (elt channel 0) #\#)
                             (funcall (intern "IRC-SAY-HELP" :lisppaste)
                                      channel))
                    (return-from cliki-lookup nil))
                  (or
                   
                   (if (string-equal first-pass "help")
                       (if (should-do-lookup first-pass (or channel sender ""))
                           (progn
                             (did-lookup first-pass (or channel sender ""))
                             (cliki-bot-help *cliki-nickname*))
                           (setf should-send-cant-find nil)))
                   (let ((strings (nth-value 1 (scan-to-strings "^(?i)help\\s+(on|about|to|describing|)\\s*\"*([^\"]+)\"*$" first-pass))))
                     (if strings
                         (if
                          (should-do-lookup first-pass (or channel sender ""))
                          (progn
                            (did-lookup first-pass (or channel sender ""))
                            (cliki-find-help (elt strings 1)))
                          (setf should-send-cant-find nil))))
                   (let ((strings (nth-value 1 (scan-to-strings "^(?i)(memo|note)\\s+(for|to)\\s+(\\S+)\\s*[:,]+\\s+(.+)$" term-with-question))))
                     (when (and sender strings)
                       (if (string-equal (without-non-alphanumeric
                                          (elt strings 2))
                                         (without-non-alphanumeric
                                          *cliki-nickname*))
                           "Buzz off."
                           (progn
                             (add-memo
                              sender
                              (if (member (elt strings 2) '("self" "myself" "me") :test #'string-equal)
                                  sender
                                  (elt strings 2))
                              (elt strings 3))
                             (format nil "Remembered. I'll tell ~A when he/she/it next speaks." (elt strings 2))))))
                   (when (and sender
                              (scan "^(?i)(discard|forget)\\s+(my\\s+|)memo(s|)$" first-pass))
                     (let ((count (remove-memos sender)))
                       (case count
                         (0 "You didn't have any memos!")
                         (1 "OK, I threw it out.")
                         (t "OK, I threw them out."))))
                   (let ((strings (nth-value 1 (scan-to-strings "^(?i)(discard|forget)\\s+(my\\s+|)memo(s|)\\s+from\\s+([^ .]+)\\.*$" first-pass))))
                     (when (and sender
                                strings)
                       (let ((count (remove-memos sender :from (elt strings 3))))
                         (case count
                           (0 "You didn't have any memos!")
                           (1 "OK, I threw it out.")
                           (t "OK, I threw them out.")))
                       ))
                   (let ((to-forget (nth-value 1 (scan-to-strings "^forget\\s+([^.]+)\\.*$" first-pass))))
                     (when to-forget
                       (forget (elt to-forget 0))
                       (format nil "What's ~A? Never heard of it." (elt to-forget 0))))
                   (let ((strs (nth-value 1 (scan-to-strings "^(?i)paste\\s+(\\d+)$" first-pass))))
                     (and strs
                          (lookup-paste (parse-integer (elt strs 0)))))
                   (let ((strings
                          (or
                           (aif
                            (nth-value 1 (scan-to-strings "^(?i)(direct|tell|show|inform|teach|give)\\s+(\\S+)\\s+(about|on|in|to|through|for|some|)\\s*(.+)$" first-pass))
                            (cons :forward it))
                           (aif
                            (nth-value 1 (scan-to-strings "^(?i)(look\\s+up\\s+|say|)\\s*(.+)\\s+(for|to|at)\\s+(\\S+)$" first-pass))
                            (cons :backward it))
                           )))
                     (if strings
                         (let* ((term (case (car strings)
                                        (:forward (elt (cdr strings) 3))
                                        (:backward (elt (cdr strings) 1))))
                                (person (case (car strings)
                                        (:forward (elt (cdr strings) 1))
                                        (:backward (elt (cdr strings) 3))))
                                (person (if (string-equal person "me")
                                            (or sender channel "you")
                                            person))
				(do-concatenate t)
                                (about
				 (handler-bind
				     ((lookup-failure
				       #'(lambda (c)
					   (setf do-concatenate nil))))
				     (cliki-lookup term :sender sender
                                                    :channel channel))))
                           (if about
			       (if do-concatenate
				   (format nil "~A: ~A~A"
					   person
					   (if (scan "http:" about)
					       (concatenate 'string
							    (random-element
							     '("have a look at"
							       "please look at"
							       "please see"
							       "direct your attention towards"
							       "look at"))
							    " ")
					       "")
					   about)
				   about)
                               (setf should-send-cant-find nil)))))
                   (if (scan "^(?i)hello(\\s|$)*" first-pass) "what's up?")
                   (if (scan "^(?i)hi(\\s|$)*" first-pass) "what's up?")
                   (if (scan "^(?i)yo(\\s|$)*" first-pass) "what's up?")
                   (if (scan "^(?i)thank(s| you)(\\s|!|\\?|\\.|$)*" first-pass)
		       (random-element
			'("you're welcome"
			  "no problem"
			  "np")))
                   (if (scan "^(?i)version(\\s|!|\\?|\\.|$)*" first-pass)
                       (format nil "This is the minion bot, running on a ~A (~A) and running under ~A ~A." (machine-type) (machine-version) (lisp-implementation-type) (lisp-implementation-version)))
                   (if (scan "^(?i)(?i)do my bidding!*$" first-pass) "Yes, my master.")
                   (if (scan "^(?i)chant(\\s|!|\\?|\\.|$)*" first-pass)
                       (format nil "MORE ~A" *more*))
                   (let ((str (nth-value 1 (scan-to-strings "^(?i)apropos\\s+(.+\\S)\\s*$" first-pass))))
                     (and str
                          (cliki-apropos (elt str 0))))
                   (if (scan "^(?i)advice$" first-pass)
                       (random-advice))
                   (let ((str (nth-value 1 (scan-to-strings "^(?i)advise\\s+(for\\s+|)(\\S+)$" first-pass))))
                     (and str
                          (format nil "~A: ~A"
                                  (if (string-equal (elt str 1) "me")
                                      (or sender channel "you")
                                      (elt str 1))
                                  (random-advice))))
                   (let ((str (nth-value 1 (scan-to-strings "^(?i)(any\\s+|some\\s+|)advi[cs]e\\s+(for\\s+|)(\\S+)\\s+(on|about)\\s+(.+)$" first-pass))))
                     (and str
                          (format nil "~A: ~A"
                                  (if (string-equal (elt str 2) "me")
                                      (or sender channel "you")
                                      (elt str 2))
                                  (search-advice (elt str 4)))))
                   (let ((str (nth-value 1 (scan-to-strings "^(?i)(any\\s+|some\\s+|)advi[cs]e\\s+(on|about)\\s+(.+)$" first-pass))))
                     (and str
                          (search-advice (elt str 2))))
                   (let ((str (nth-value 1 (scan-to-strings "^(?i)advice\\W+(\\d+)$" first-pass))))
                     (and str
                          (lookup-advice (elt str 0))))
		   (let ((str
			  (or
			   (nth-value 1 (scan-to-strings "^(?i)what\\s+does\\s+([a-zA-Z\"]+)\\s+(mean|stand\\s+for)$" first-pass))
			   (nth-value 1 (scan-to-strings "^(?i)what\\s+([a-zA-Z\"]+)\\s+(means|stands\\s+for)$" first-pass)))))
		     (and str
			  (let ((letters (remove #\" (elt str 0))))
			    (when (< (length letters) 9)
			      (without-abuse
				  (if (and (> (length letters) 2)
					   (string-equal (subseq letters (- (length letters) 2)) "cl"))
				      (steel-bazooka:steel-whatever :letters (string-downcase (subseq letters 0 (- (length letters) 2))))
				      (steel-bazooka:steel-whatever :letters (string-downcase letters) :suffix nil)))))))
		   (let ((str (nth-value 1 (scan-to-strings "^(?i)shorten\\s+(\\w+://.+\\S)\\s*$" term-with-question))))
                     (and str
                          (shorten (elt str 0))))
                   (if (should-do-lookup first-pass (or channel sender ""))
                       (aif (or (small-definition-lookup first-pass)
                                (cliki-first-sentence first-pass)
                                (alias-lookup first-pass))
                            (prog1
                                (concatenate 'string first-pass ": " it)
                              (did-lookup first-pass (or channel sender ""))))
                       (setf should-send-cant-find nil))
                   (if (and
                        should-send-cant-find
                        (or
                         (scan "(!|\\.|\\s.+\\?|\\)|\\()\\s*$" term-with-question)
                         (scan "^\\s*\\S+\\s+\\S+.*$" term-with-question)))
                       ;;(generate-text (+ 20 (random 6)))
                       (progn
                         (setf should-send-cant-find nil)
                         (do-eliza first-pass))
                       )
                   (when should-send-cant-find
		     (signal 'lookup-failure)
                     (format nil "Sorry, I couldn't find anything in the database for ``~A''.~A" first-pass (if (scan " " first-pass) " Maybe you meant to end with punctuation?" "")))
                   ))))))))



(defun valid-cliki-message (message)
  (scan *cliki-attention-prefix* (car (last (arguments message)))))

(defvar *respond-to-general-hellos* nil)

(defun anybody-here (string)
  (if *respond-to-general-hellos*
      (or (scan "(?i)(anybody|aynbody|any body|anyone|aynone|any one|ne1|any1|n e 1|ne 1) (here|awake|there|home|know).*\\?*" string)
	  (scan "^(?i)\\s*(hello|hi|yo)\\s*(channel|room|people|ppl|all|peeps|)\\s*$" string))))

(defun msg-hook (message)
  (handler-bind
      ((serious-condition (lambda (c)
	 (format *trace-output* "Caught error: ~A~%" c)
	 #+nil (sb-debug:backtrace 10 *trace-output*)
	 (format *trace-output* "~A~%"
		 (nthcdr 10 (sb-debug:backtrace-as-list)))
	 (return-from msg-hook))))
    (progn
      (scan-for-more (car (last (arguments message))))
      (let ((respond-to (if (string-equal (first (arguments message)) *cliki-nickname*) (source message) (first (arguments message)))))
	(if (valid-cliki-message message)
	    (let ((response (cliki-lookup (regex-replace *cliki-attention-prefix* (car (last (arguments message))) "") :sender (source message) :channel (first (irc:arguments message)))))
	      (and response (privmsg *cliki-connection* respond-to response)))
	    (if (string-equal (first (arguments message)) *cliki-nickname*)
		(aif (cliki-lookup (car (last (arguments message))) :sender (source message))
		     (privmsg *cliki-connection* respond-to it))
		(if (anybody-here (car (last (arguments message))))
		    (privmsg *cliki-connection* (first (arguments message)) (format nil "~A: hello." (source message))))))
	(take-care-of-memos respond-to (source message))))))

(defvar *cliki-nickserv-password* "")

(defun notice-hook (message)
  (if (and (string-equal (source message) "NickServ")
	   (scan "owned by someone else" (car (last (arguments message)))))
      (privmsg *cliki-connection* (source message) (format nil "IDENTIFY ~A" *cliki-nickserv-password*))))

(defun rename-cliki (new-nick)
  (setf *cliki-nickname* new-nick)
  (nick *cliki-connection* new-nick)
  (setf *cliki-attention-prefix* (make-cliki-attention-prefix new-nick)))

(defun start-cliki-bot (nick server &rest channels)
  (read-small-definitions)
  (setf *cliki-nickname* nick)
  (setf *cliki-connection* (connect :nickname *cliki-nickname* :server server))
  (setf *cliki-attention-prefix* (make-cliki-attention-prefix nick))
  (mapcar #'(lambda (channel) (join *cliki-connection* channel)) channels)
  (add-hook *cliki-connection* 'irc::irc-privmsg-message 'msg-hook)
  (add-hook *cliki-connection* 'irc::irc-notice-message 'notice-hook)
  (start-background-message-handler *cliki-connection*))

(defun shuffle-hooks ()
  (irc::remove-hooks *cliki-connection* 'irc::irc-privmsg-message)
  (add-hook *cliki-connection* 'irc::irc-privmsg-message 'msg-hook))
