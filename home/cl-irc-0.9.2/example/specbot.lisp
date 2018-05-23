;;;; $Id: specbot.lisp 211 2008-06-30 21:29:32Z ehuelsmann $
;;;; $Source$

;;;; specbot.lisp - an example IRC bot for cl-irc

;;; specbot is an example IRC bot for cl-irc. It runs on
;;; irc.freenode.net in the channels #lisp, #scheme and #clhs
;;; (preferred for testing). It responds to queries of its various
;;; databases, which right now include "clhs" and "r5rs".

;;; You will need to load and populate the tables for both the clhs
;;; and r5rs lookup packages; currently these are available in
;;; lisppaste CVS.

;;; To use it, load the cl-irc system, load specbot.lisp, and
;;; invoke (specbot:start-specbot "desirednickname" "desiredserver"
;;; "#channel1" "#channel2" "#channel3" ...)

(defpackage :specbot (:use :common-lisp :irc) (:export :start-specbot
                                                       :shut-up
                                                       :un-shut-up))
(in-package :specbot)

(defvar *connection*)
(defvar *nickname* "")

(defun shut-up ()
  (setf (irc:client-stream *connection*) (make-broadcast-stream)))

(defun un-shut-up ()
  (setf (irc:client-stream *connection*) *trace-output*))

(defmacro aif (test conseq &optional (else nil))
  `(let ((it ,test))
     (if it ,conseq
       (symbol-macrolet ((it ,test))
         ,else))))

(defun clhs-lookup (str)
  (and (find-package :clhs-lookup)
       (funcall (intern "SPEC-LOOKUP" :clhs-lookup)
                str)))

(defun r5rs-lookup (str)
  (and (find-package :r5rs-lookup)
       (funcall (intern "SYMBOL-LOOKUP" :r5rs-lookup)
                str)))

(defun cocoa-lookup (str)
  (and (find-package :cocoa-lookup)
       (funcall (intern "SYMBOL-LOOKUP" :cocoa-lookup)
                str)))

(defun elisp-lookup (str)
  (and (find-package :elisp-lookup)
       (funcall (intern "SYMBOL-LOOKUP" :elisp-lookup)
                str)))

(defun clim-lookup (str)
  (and (find-package :clim-lookup)
       (funcall (intern "TERM-LOOKUP" :clim-lookup)
                str)))

(defvar *spec-providers*
  '((clhs-lookup "clhs" "The Common Lisp HyperSpec")
    (r5rs-lookup "r5rs" "The Revised 5th Ed. Report on the Algorithmic Language Scheme")
    (cocoa-lookup "cocoa" "Classes in the Cocoa Foundation and Application kits")
    (elisp-lookup "elisp" "GNU Emacs Lisp Reference Manual")
    (clim-lookup "clim" "Common Lisp Interface Manager II Specification")))

(defvar *spaces-allowed*
  '(clim-lookup))

(defvar *alists* nil)

(defun add-simple-alist-lookup (file designator prefix description)
  (unless (assoc designator *alists*)
    (let ((alist (with-open-file (s file :direction :input) (read s))))
      (push (cons designator alist) *alists*)
      (setf *spec-providers*
            (nconc *spec-providers*
                   (list `((simple-alist-lookup ,designator) ,prefix ,description)))))))

(defun simple-alist-lookup (designator string)
  (let ((alist (cdr (assoc designator *alists*))))
    (cdr (assoc string alist :test #'equalp))))

(defun valid-message (string prefix &key space-allowed)
  (if (eql (search prefix string :test #'char-equal) 0)
      (and (or space-allowed
               (not (find #\space string :start (length prefix))))
           (length prefix))
      nil))

(defun strip-address (string &key (address *nickname*) (final nil))
  (loop for i in (list (format nil "~A " address)
                       (format nil "~A: " address)
                       (format nil "~A:" address)
                       (format nil "~A, " address))
        do (aif (valid-message string i :space-allowed t)
                (return-from strip-address (subseq string it))))
  (and (not final) string))

(defun msg-hook (message)
  (let ((destination (if (string-equal (first (arguments message)) *nickname*)
                         (source message)
                         (first (arguments message))))
        (to-lookup (strip-address (car (last (arguments message))))))
    (if (and (or
              (string-equal (first (arguments message)) *nickname*)
              (not (string= to-lookup (car (last (arguments message))))))
             (member to-lookup '("help" "help?") :test #'string-equal))
        (progn
          (privmsg *connection* destination
                   (format nil "To use the ~A bot, say something like \"database term\", where database is one of (~{~S~^, ~}) and term is the desired lookup. The available databases are:"
                           *nickname*
                           (mapcar #'second *spec-providers*)))
          (loop for i from 1 for j in *spec-providers*
                with elts = nil
                do (push j elts)
                if (zerop (mod i 4))
                do (progn
                     (privmsg *connection* destination
                              (format nil "~{~{~*~S, ~A~}~^; ~}"
                                      (nreverse elts)))
                     (setf elts nil)))
	  )
        (loop for type in *spec-providers*
              for actual-fun = (if (typep (first type) 'symbol)
                                   (first type)
                                   (lambda (lookup) (destructuring-bind (fun first-arg) (first type)
                                                      (funcall fun first-arg lookup))))
              do
              (aif (strip-address to-lookup :address (second type) :final t)
                   (let ((looked-up (funcall actual-fun it)))
                     (if (and (<= 0 (count #\space it)
				  (if (member actual-fun *spaces-allowed*) 1 0)1)
                              (not looked-up))
                         (setf looked-up (format nil "Sorry, I couldn't find anything for ~A."  it)))
                     (and looked-up
                          (privmsg *connection* destination looked-up))))))))

(defparameter *754-file*
  (merge-pathnames "754.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     (or *load-truename*
                         *default-pathname-defaults*)))))

(defparameter *ppc-file*
  (merge-pathnames "ppc-assem.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     (or *load-truename*
                         *default-pathname-defaults*)))))

(defparameter *sus-file*
  (merge-pathnames "sus.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     (or *load-truename*
                         *default-pathname-defaults*)))))

(defparameter *man-file*
  (merge-pathnames "man.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     (or *load-truename*
                         *default-pathname-defaults*)))))

(defun start-specbot (nick server &rest channels)
  (add-simple-alist-lookup *754-file* 'ieee754 "ieee754" "Section numbers of IEEE 754")
  (add-simple-alist-lookup *ppc-file* 'ppc "ppc" "PowerPC assembly mnemonics")
  (add-simple-alist-lookup *sus-file* 'sus "posix" "Single UNIX Specification")
  (add-simple-alist-lookup *man-file* 'man "man" "Mac OS X Man Pages")
  (setf *nickname* nick)
  (setf *connection* (connect :nickname *nickname* :server server))
  (mapcar #'(lambda (channel) (join *connection* channel)) channels)
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  #+(or sbcl
        openmcl)
  (start-background-message-handler *connection*)
  #-(or sbcl
        openmcl)
  (read-message-loop *connection*))

(defun shuffle-hooks ()
  (irc::remove-hooks *connection* 'irc::irc-privmsg-message)
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook))
