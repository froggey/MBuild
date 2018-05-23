;;;; $Id: utility.lisp 239 2013-01-13 18:59:49Z ehuelsmann $
;;;; $URL: file:///project/cl-irc/svn/tags/0.9.2/utility.lisp $

;;;; See the LICENSE file for licensing information.

(in-package :irc)

(defun get-day-name (day-number)
  "Given a number, such as 1, return the appropriate day name,
abbrevated, such as \"Tue\".  Index 0 is Monday."
  (case day-number
    (0 "Mon")
    (1 "Tue")
    (2 "Wed")
    (3 "Thu")
    (4 "Fri")
    (5 "Sat")
    (6 "Sun")
    (otherwise
     (error "Unknown day ~A." day-number))))

(defun get-month-name (month-number)
  "Index 1 is January."
  (case month-number
    (1 "Jan")
    (2 "Feb")
    (3 "Mar")
    (4 "Apr")
    (5 "May")
    (6 "Jun")
    (7 "Jul")
    (8 "Aug")
    (9 "Sep")
    (10 "Oct")
    (11 "Nov")
    (12 "Dec")
    (otherwise
     (error "Unknown month ~A." month-number))))

(defun make-time-message (second minute hour date month year day)
  "Returns a string composed of the input parameters so that it
represents a time message as by the IRC protocol."
  (format nil "~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~D"
          (get-day-name day)
          (get-month-name month)
          date
          hour
          minute
          second
          year))

(defun make-irc-message (command &rest arguments)
  "Return a valid IRC message, as a string, composed of the input
parameters."
  (let ((*print-circle* nil))
    (format nil
            "~A~{ ~A~}~@[ :~A~]~%"
            command (butlast arguments) (car (last arguments)))))

(defun make-ctcp-message (string)
  "Return a valid IRC CTCP message, as a string, composed by
`string'."
  (format nil "~A~A~A" +soh+ string +soh+))

(defun tokenize-string (string &key
                               (delimiters '(#\Space #\Return #\Linefeed #\Newline)))
  "Split string into a list, splitting on `delimiters' and removing any
empty subsequences."
  (split-sequence:split-sequence-if #'(lambda (character)
                                        (member character delimiters))
                                    string :remove-empty-subseqs t))

(defun list-of-strings-to-integers (list)
  "Take a list of strings and return a new list of integers (from
parse-integer) on each of the string elements."
  (let ((new-list nil))
    (dolist (element (reverse list))
      (push (parse-integer element) new-list))
    new-list))

(defun host-byte-order (string)
  "Convert a string, such as 192.168.1.1, to host-byte-order, such as
3232235777."
  (let ((list (list-of-strings-to-integers (split-sequence:split-sequence #\. string))))
    (+ (* (first list) 256 256 256) (* (second list) 256 256)
       (* (third list) 256) (fourth list))))

(defun hbo-to-dotted-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (format nil "~A.~A.~A.~A" first second third fourth)))

(defun hbo-to-vector-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (vector first second third fourth)))

(defun external-format-fixup (format)
  (let ((new-format (copy-list format)))
    (setf (getf (cdr new-format) :eol-style) :crlf)
    new-format))

(defun try-decode-line (line external-formats)
  (loop for external-format in external-formats
        for decoded = nil
        for error = nil
        do (multiple-value-setq (decoded error)
             (handler-case
              (flexi-streams:with-input-from-sequence (in line)
                (let* ((ex-fmt (external-format-fixup external-format))
                       (flexi (flexi-streams:make-flexi-stream
                               in
                               ;; :element-type 'character
                               :external-format ex-fmt)))
                  (read-line flexi nil nil)))
              (flexi-streams:external-format-encoding-error ()
                  nil)))
        if decoded
        do (return decoded)))

(defun read-byte-no-hang (stream &optional eof-error-p eof-value)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (when (listen stream)
    (read-byte stream eof-error-p eof-value)))

(defun read-sequence-until (stream target limit &key non-blocking)
  "Reads data from `stream' into `target' until the subsequence
`limit' is reached or `target' is not large enough to hold the data."
  (let ((read-fun (if (subtypep (stream-element-type stream) 'integer)
                      (if non-blocking #'read-byte-no-hang #'read-byte)
                    (if non-blocking #'read-char-no-hang #'read-char)))
        (limit-vector (coerce limit '(vector t *)))
        (targ-max (1- (length target)))
        (limit-max (length limit))
        (limit-cur 0)
        (targ-cur -1))
    (declare (optimize (speed 3) (debug 0))
             (type fixnum targ-cur))
    ;; In SBCL read-char is a buffered operations (depending on
    ;; stream creation parameters), so this loop should be quite efficient
    ;; For others, if this becomes an efficiency problem, please report...
    (loop for next-elt = (funcall read-fun stream nil nil)
          if (null next-elt)
          do (return (values target (1+ targ-cur) t))
          else do
          (setf (elt target (incf targ-cur)) next-elt)
          (if (eql next-elt (aref limit-vector limit-cur))
              (incf limit-cur)
            (setf limit-cur 0))

          if (or (= targ-cur targ-max)
                 (= limit-cur limit-max))
          do (return (values target (1+ targ-cur) nil)))))

(defun read-protocol-line (connection)
  "Reads a line from the input network stream, returning a
character array with the input read."
  (multiple-value-bind
      (buf buf-len)
      ;; Note: we cannot use read-line here (or any other
      ;; character based functions), since they may cause
      ;; (at this time unwanted) character conversion
      (read-sequence-until (network-stream connection)
                           (make-array 1024
                                       :element-type '(unsigned-byte 8)
                                       :fill-pointer t)
                           '(10))
    (when (< 0 buf-len)
      (setf (fill-pointer buf)
            ;; remove all trailing CR and LF characters
            ;; (This allows non-conforming clients to send CRCRLF
            ;;  as a line separator too).
            (or (position-if #'(lambda (x) (member x '(10 13)))
                             buf :from-end t :end buf-len)
                buf-len))
      (try-decode-line buf *default-incoming-external-formats*))))

(defmacro dynfound-funcall ((symbol-name &optional package) &rest parameters)
  (let ((package-sym (gensym))
        (symbol-sym (gensym))
        (fun-sym (gensym)))
    `(let* ((,package-sym ,(if package package *package*))
            (,symbol-sym ,(if (symbolp symbol-name)
                              `',symbol-name
                            symbol-name))
            (,symbol-sym (find-symbol
                          ,(if (symbolp symbol-name)
                               `(symbol-name ,symbol-sym)
                             `(if (symbolp ,symbol-sym)
                                  (symbol-name ,symbol-sym)
                                ,symbol-sym))
                          ,package-sym))
            (,fun-sym (when (and ,symbol-sym (fboundp ,symbol-sym))
                        (symbol-function ,symbol-sym))))
       (unless ,symbol-sym
         (error "Can't resolve symbol ~A in package ~A"
                ,symbol-sym ,package-sym))
       (if ,fun-sym
           (funcall ,fun-sym ,@parameters)
         (error "Symbol ~A in package ~A isn't fbound"
                ,symbol-sym ,package-sym)))))

(defun substring (string start &optional end)
  (let* ((end-index (if end end (length string)))
         (seq-len (- end-index start)))
    (make-array seq-len
                :element-type (array-element-type string)
                :displaced-to string
                :displaced-index-offset start)))


(defun cut-between (string start-char end-chars
                           &key (start 0) (cut-extra t) (cut-to-end nil))
  "If `start-char' is not nil, cut string between `start-char' and any
of the `end-chars', from `start'.  If `start-char' is nil, cut from
`start' until any of the `end-chars' (or sting-end when `cut-to-end' is true).

If `cut-extra' is t, we will cut from start + 1 instead of just
`start'.

When there is no string matching the input parameters `start' and nil
will be returned, otherwise `end-position' and the string are
returned."
  (let ((end-position (or (position-if #'(lambda (char)
                                           (member char end-chars))
                                       string :start (1+ start))
                          (when cut-to-end (length string))))
        (cut-from (if cut-extra
                      (1+ start)
                      start)))
    (if (and end-position start-char)
        (if (eql (char string start) start-char)
            (values end-position
                    (substring string cut-from end-position))
            (values start nil))
        (if end-position
            (values end-position
                    (substring string cut-from end-position))
            (values start nil)))))

(defun cut-before (string substring end-chars
                          &key (start 0) (cut-extra t) (cut-to-end nil))
  "Cut `string' before `substring' or any of the `end-chars', from `start',
if none of substring or end-chars are found, until the end of the string
when `cut-to-end' is true.

If `cut-extra' is t, we will cut from start + 1 instead of just
`start'.

When there is no string matching the input parameters `start' and nil
will be returned, otherwise `end-position' and the string are
returned."
  (let ((end-position (search substring string :start2 start)))
    (if end-position
        (values (+ end-position (1- (length substring)))
                (substring string (if (and cut-extra
                                        (< start end-position))
                                   (1+ start) start) end-position))
      (let ((end-position (or (position-if #'(lambda (x)
                                               (member x end-chars))
                                           string :start (1+ start))
                              (when cut-to-end (length string))))
            (cut-from (if cut-extra (1+ start) start)))
        (if end-position
            (values end-position
                    (substring string cut-from end-position))
          (values start nil))))))


;;
;; Message arguments binding macro
;;

(defmacro destructuring-arguments (lambda-list message &body body)
  "Destructures the `arguments' slot in `message' according
to `lambda-list' and binds them in `body'.

The lambda list syntax is as follows:

wholevar::= &whole var
reqvars::= var*
optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}* ]
restvar::= [&rest var]
reqtrailingvars::= [&req var*]
lambda-list::= (wholevar reqvars optvars restvar reqtrailingvars)

With the exception of &req (which is new) and &rest, all lambda list
keywords are analogous to a destructuring lambda list (see clhs 3.4.5).

If &req is specified, these values are consumed off the end of the list
before processing any preceeding &optional or &rest keywords.

For any variable, the `:ignored' keyword can be passed instead,
indicating the binding should be ignored in the `body'."
  (let ((%message (gensym))
        (%args (gensym))
        (%arg-count (gensym))
        (valid-keywords '(&whole &optional &rest &req)))
    (labels ((lambda-key-p (x)
                (member x valid-keywords))
             (ignored-p (x)
                (eq x :ignored))
             (count-valid-keys (lambda-list)
                (count-if #'lambda-key-p lambda-list))
             (replace-ignored (lambda-list)
                (let ((ignores))
                  (values (mapcar #'(lambda (x)
                                      (if (ignored-p x)
                                          (let ((y (gensym)))
                                            (push y ignores)
                                            y)
                                        x))
                                  lambda-list)
                          ignores)))
             (bind-req-trail (req-trail args body)
                (let ((req-syms (cdr req-trail)))
                  (if (and req-trail
                           (notevery #'ignored-p req-syms))
                      (multiple-value-bind
                          (ll ignores) (replace-ignored req-syms)
                        `(destructuring-bind
                             ,ll ,args
                           ,(if ignores
                                `(declare (ignore ,@ignores))
                              (values))
                           ,body))
                    body))))

      (let* ((whole-var (when (eq (car lambda-list) '&whole)
                          (second lambda-list)))
             (lambda-list (if whole-var (nthcdr 2 lambda-list) lambda-list))
             (opt-entries (member '&optional lambda-list))
             (rest-entries (member '&rest lambda-list))
             (req-trail (member '&req lambda-list))
             (destructuring-ll (butlast lambda-list (length req-trail)))
             (longest-sublist (cond
                               (opt-entries opt-entries)
                               (rest-entries rest-entries)
                               (req-trail req-trail)
                               (t nil)))
             (min-entries (+ (if req-trail (1- (length req-trail)) 0)
                             ;; required start && end
                             (- (- (length lambda-list)
                                   (count-valid-keys lambda-list))
                                (- (length longest-sublist)
                                   (count-valid-keys longest-sublist)))))
             (max-entries (when (null rest-entries)
                            ;; required start && end && optionals
                            (+ min-entries
                               (if opt-entries
                                   (- (1- (length opt-entries))
                                      (length req-trail))
                                 0)))))

        `(let* ((,%message ,message)
                (,%args (arguments ,%message))
                (,%arg-count (length ,%args))
                ,@(if (and whole-var
                           (not (ignored-p whole-var)))
                      `((,whole-var ,%args))
                    (values)))
           (when ,(if max-entries
                       `(not (and (<= ,min-entries ,%arg-count)
                                  (<= ,%arg-count ,max-entries)))
                     `(> ,min-entries ,%arg-count))
             ;; we want to raise a cl-irc condition here!
             (error (format nil "Unexpected protocol input; provided arguments ~
~S don't match with expected arguments ~S" ',lambda-list ,%args)))
           ,(bind-req-trail
              req-trail
              `(last ,%args ,(1- (length req-trail)))
              (multiple-value-bind
                  (ll ignores) (replace-ignored destructuring-ll)
                `(destructuring-bind
                     ,ll
                     ,(if req-trail
                          `(butlast ,%args ,(1- (length req-trail)))
                        %args)
                   ,(if ignores
                        `(declare (ignore ,@ignores))
                      (values))
                   ,@body))))))))


;;
;; RPL_ISUPPORT support routines
;;

(defun parse-isupport-prefix-argument (prefix)
  (declare (type string prefix))
  (let ((closing-paren-pos (position #\) prefix)))
    (when (and (eq (elt prefix 0) #\( )
               closing-paren-pos)
      (let ((prefixes (substring prefix (1+ closing-paren-pos)))
            (modes (substring prefix 1 closing-paren-pos)))
        (when (= (length prefixes)
                 (length modes))
          (values prefixes modes))))))

(defun nick-prefixes-from-isupport (isupport-arguments)
  "Returns an assoc list associating prefix characters with mode characters."
  (multiple-value-bind
      (prefixes modes)
      (parse-isupport-prefix-argument (second (assoc "PREFIX"
                                                     isupport-arguments
                                                     :test #'string=)))
    (let ((rv))
      (dotimes (i (length modes)
                  rv)
        (setf (getf rv (char prefixes i))
              (char modes i))))))

(defun chanmode-descs-from-isupport (isupport-arguments
                                     &optional
                                     (mode-symbols
                                      *default-char-to-channel-modes-map*))
  "Parses a string describing channel modes conforming to
http://www.irc.org/tech_docs/draft-brocklesby-irc-isupport-03.txt
paragraph 3.3.

It returns a list of mode-description records."
  (let* ((mode-desc-recs)
         (pref (second (assoc "PREFIX" isupport-arguments :test #'string=)))
         (chanmodes (second (assoc "CHANMODES" isupport-arguments
                                   :test #'string=)))
         (modes-list
          (cons (second (multiple-value-list
                         (parse-isupport-prefix-argument pref)))
                (split-sequence:split-sequence #\, chanmodes)))
         (mode-descs '(;; B type mode from PREFIX with nick argument
                       (t t t list-value-mode)
                       ;; A type mode
                       (:optional-for-server
                        :optional-for-server nil list-value-mode)
                       ;; B type mode from CHANMODES
                       (t   t   nil single-value-mode)
                       ;; C type mode from CHANMODES
                       (t   nil nil single-value-mode)
                       ;; D type mode from CHANMODES
                       (nil nil nil boolean-value-mode))))
    (do ((mode (pop modes-list) (pop modes-list))
         (mode-desc (pop mode-descs) (pop mode-descs)))
        ((null mode-desc) mode-desc-recs)
      (when (< 0 (length mode))
        (let ((mode-struct
               (make-mode-description :param-on-set-p (first mode-desc)
                                      :param-on-unset-p (second mode-desc)
                                      :nick-param-p (third mode-desc)
                                      :class (fourth mode-desc))))
          (dotimes (j (length mode))
            (let ((mode-rec (copy-structure mode-struct))
                  (mode-char (elt mode j)))
              (setf (mode-desc-char mode-rec) mode-char
                    (mode-desc-symbol mode-rec) (cdr (assoc mode-char
                                                            mode-symbols)))
              (push mode-rec mode-desc-recs))))))))

(defmacro do-property-list ((prop val list) &body body)
  (let ((lsym (gensym)))
    `(let ((,lsym ,list))
       (do* ((,prop (pop ,lsym) (pop ,lsym))
             (,val (pop ,lsym) (pop ,lsym)))
           ((and (null ,lsym)
                 (null ,prop)
                 (null ,val)))
         ,@body))))

(defgeneric irc-string-downcase (map-name string &key start end))

(defmethod irc-string-downcase (map-name
                                string &key (start 0) end)
  (declare (ignore map-name))
  (let* ((new-string (substitute #\[ #\{ string :start start :end end))
         (new-string (substitute #\] #\} new-string :start start :end end))
         (new-string (substitute #\\ #\| new-string :start start :end end))
         (new-string (substitute #\~ #\^ new-string :start start :end end)))
    (string-downcase new-string :start start :end end)))

(defmethod irc-string-downcase ((map-name (eql :ascii))
                                string &key (start 0) end)
  (declare (ignore map-name))
  (string-downcase string :start start :end end))

(defun parse-isupport-multivalue-argument (argument)
  (declare (type string argument))
  (mapcar #'(lambda (x)
              (split-sequence:split-sequence #\: x))
          (split-sequence:split-sequence #\, argument)))

(defun apply-mode-changes (connection target mode-arguments server-p)
  (dolist (change (parse-mode-arguments connection target mode-arguments
                                        :server-p server-p))
    (apply-mode-change connection target change)))

(defun apply-mode-change (connection target change)
  (destructuring-bind
        (op mode-name value)
      change
    (unless (has-mode-p target mode-name)
      (add-mode target mode-name
                (make-mode connection target mode-name)))
    (funcall (if (char= #\+ op) #'set-mode #'unset-mode)
             target mode-name value)))

(defun parse-mode-arguments (connection target arguments &key server-p)
  "Create a list of mode changes with their arguments for `target'
   from `mode-string' and `arguments'.

   Throw nil to the UNKNOWN-MODE symbol if any of the mode chars are unknown."
  (catch 'illegal-mode-spec
    (if (and (= 1 (length arguments))
             (null (position (char (first arguments) 0) "+-")))
        ;; type 1 mode specification; only allowed on servers
        (when server-p
          (let ((ops)
                (arg (car arguments)))
            (dotimes (i (length arg) (reverse ops))
              (push (char arg i) ops))))
      ;; type 2 mode specification; clients and servers
      (let ((ops))
        (do ((changes (pop arguments) (pop arguments)))
            ((null changes) (values ops nil))
          (let* ((this-op (char changes 0))
                 (modes (substring changes 1))
                 (param-req (if (char= this-op #\+)
                                #'mode-desc-param-on-set-p
                              #'mode-desc-param-on-unset-p)))
            (unless (position this-op "+-")
              (throw 'illegal-mode-spec nil))
            (dotimes (i (length modes))
              (case (char modes i)
                ((#\+ #\-) (setf this-op (char modes i)))
                (t 
                 (let* ((mode-rec
                         (mode-description connection target
                                           (mode-name-from-char connection target
                                                                (char modes i))))
                        (param-p (when mode-rec
                                   (funcall param-req mode-rec))))
                   (when (or (null mode-rec)
                             (and param-p
                                  (= 0 (length arguments))))
                     (throw 'illegal-mode-spec nil))
                   (push (list this-op
                               (mode-desc-symbol mode-rec)
                               (when param-p
                                 (if (mode-desc-nick-param-p mode-rec)
                                     (find-user connection (pop arguments))
                                     (pop arguments)))) ops)))))))))))


;;;
;;; Hostmask matcher
;;;

(defun do-mask-match (mask hostname mask-consumed host-consumed)
  (if (= (length mask) (1+ mask-consumed))
      ;; we're out of mask to match, hopefully, we're out of hostname too
      (= (length hostname) (1+ host-consumed))
    (let ((mask-char (char mask (1+ mask-consumed))))
      (cond
       ((eq mask-char #\?)
        ;; match any character, if there is one
        (do-mask-match mask hostname (1+ mask-consumed) (1+ host-consumed)))
       ((eq mask-char #\*)
        ;; match any number of characters (including zero)
        (do ((match (do-mask-match mask hostname
                                   (incf mask-consumed)
                                   host-consumed)
                    (do-mask-match mask hostname
                                   mask-consumed
                                   (incf host-consumed))))
            ((or (= (length hostname) (1+ host-consumed))
                 match)
             match)))
       ((= (1+ host-consumed) (length hostname))
        ;; we're out of hostname...
        nil)
       (t
        ;; match other characters by exact matches
        (when (eq mask-char (char hostname (1+ host-consumed)))
          (do-mask-match mask hostname
                         (1+ mask-consumed) (1+ host-consumed))))))))

  (defun mask-matches-p (mask hostname)
    "Wildcard matching.

Uses `*' to match any number of characters and `?' to match exactly any
one character.  The routine does not enforce hostmask matching patterns,
but can be used for the purpose."
    (do-mask-match mask hostname -1 -1))
