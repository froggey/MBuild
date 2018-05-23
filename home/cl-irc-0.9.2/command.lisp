;;;; $Id: command.lisp 238 2013-01-13 18:50:37Z ehuelsmann $
;;;; $URL: file:///project/cl-irc/svn/tags/0.9.2/command.lisp $

;;;; See LICENSE for licensing information.

(in-package :irc)

(defgeneric pass (connection password))
(defgeneric nick (connection new-nickname))
(defgeneric user- (connection username mode &optional realname))
(defgeneric oper (connection name password))
(defgeneric mode (connection nickname &optional mode))
(defgeneric op (connection channel nickname))
(defgeneric deop (connection channel nickname))
(defgeneric voice (connection channel user))
(defgeneric devoice (connection channel nickname))
(defgeneric ban (connection channel mask))
(defgeneric unban (connection channel mask))
(defgeneric service (connection nickname distribution info))
(defgeneric quit (connection &optional message))
(defgeneric squit (connection server comment))
(defgeneric join (connection channel &key password))
(defgeneric multi-join (connection channels))
(defgeneric part (connection channel &optional reason))
(defgeneric part-all (connection &optional reason))
(defgeneric topic- (connection channel topic))
(defgeneric names (connection channel &optional target))
(defgeneric list- (connection &optional channel target))
(defgeneric invite (connection user channel))
(defgeneric kick (connection channel user &optional comment))
(defgeneric privmsg (connection channel message))
(defgeneric notice (connection target message))
(defgeneric motd- (connection &optional target))
(defgeneric lusers (connection &optional mask target))
(defgeneric version (connection &optional target))
(defgeneric stats (connection &optional query target))
(defgeneric links (connection &optional remote-server server-mask))
(defgeneric time- (connection &optional target))
(defgeneric trace- (connection &optional target))
(defgeneric admin (connection &optional target))
(defgeneric info (connection &optional target))
(defgeneric servlist (connection &optional mask type))
(defgeneric squery (connection service-name text))
(defgeneric who (connection &optional mask o))
(defgeneric whois (connection mask &optional target))
(defgeneric whowas (connection nickname &optional count target))
(defgeneric kill (connection user &optional comment))
(defgeneric ping (connection server))
(defgeneric pong (connection server &optional server2))
(defgeneric error- (connection message))
(defgeneric away (connection message))
(defgeneric rehash (connection))
(defgeneric die (connection))
(defgeneric restart- (connection))
(defgeneric summon (connection nickname &optional target channel))
(defgeneric users- (connection &optional target))
(defgeneric wallops (connection message))
(defgeneric userhost (connection nickname))
(defgeneric ison (connection user))
(defgeneric action (connection target message))
(defgeneric ctcp (connection target message))
(defgeneric ctcp-reply (connection target message))
(defgeneric ctcp-chat-initiate (connection nickname &key passive)
  (:documentation "Initiate a DCC chat session with `nickname' associated
with `connection'.

If `passive' is non-NIL, the remote is requested to serve as a DCC
host. Otherwise, the local system will serve as a DCC host.  The
latter may be a problem for firewalled or NATted hosts."))
(defgeneric dcc-request-accept (message)
  (:documentation ""))
(defgeneric dcc-request-reject (message &optional reason)
  (:documentation ""))
(defgeneric dcc-request-cancel (connection token)
  (:documentation ""))


(defmethod pass ((connection connection) (password string))
  "A \"PASS\" command is not required for a client connection to be
registered, but it MUST precede the latter of the NICK/USER
combination (for a user connection) or the SERVICE command (for a
service connection). The RECOMMENDED order for a client to register is
as follows:

                           1. Pass message
           2. Nick message                 2. Service message
           3. User message

Upon success, the client will receive an RPL_WELCOME (for users) or
RPL_YOURESERVICE (for services) message indicating that the connection
is now registered and known the to the entire IRC network.  The reply
message MUST contain the full client identifier upon which it was
registered."
  (send-irc-message connection :pass password))

(defmethod nick ((connection connection) (new-nickname string))
  (send-irc-message connection :nick new-nickname))

(defmethod user- ((connection connection) (username string)
                  (mode integer) &optional (realname ""))
  (send-irc-message connection :user username mode "*" realname))

(defmethod oper ((connection connection) (name string) (password string))
  (send-irc-message connection :oper name password))

(defmethod mode ((connection connection) (nickname string) &optional mode)
  (send-irc-message connection :mode nickname mode))

;; utility functions not part of the RFCs
(defmethod op ((connection connection) (channel string) (nickname string))
  (send-irc-message connection :mode channel "+o" nickname))

(defmethod op ((connection connection) (channel channel) (user user))
  (op connection (name channel) (nickname user)))

(defmethod deop ((connection connection) (channel string) (nickname string))
  (send-irc-message connection :mode channel "-o" nickname))

(defmethod deop ((connection connection) (channel channel) (user user))
  (deop connection (name channel) (nickname user)))

(defmethod voice ((connection connection) (channel string) (nickname string))
  (send-irc-message connection :mode channel "+v" nickname))

(defmethod voice ((connection connection) (channel channel) (user user))
  (voice connection (name channel) (nickname user)))

(defmethod devoice ((connection connection) (channel string) (nickname string))
  (send-irc-message connection :mode channel "-v" nickname))

(defmethod devoice ((connection connection) (channel channel) (user user))
  (devoice connection (name channel) (nickname user)))

(defmethod ban ((connection connection) (channel string) (mask string))
  (send-irc-message connection :mode channel "+b" mask))

(defmethod ban ((connection connection) (channel channel) (mask string))
  (ban connection (name channel) mask))

;; unban or deban?
(defmethod unban ((connection connection) (channel string) (mask string))
  (send-irc-message connection :mode channel "-b" mask))

(defmethod unban ((connection connection) (channel channel) (mask string))
  (unban connection (name channel) mask))

(defmethod service ((connection connection) (nickname string)
                    (distribution string) (info string))
  (send-irc-message connection :service nickname "*" distribution 0 0 info))

(defmethod quit ((connection connection) &optional (message *default-quit-message*))
  (remove-all-channels connection)
  (remove-all-users connection)
  (dolist (dcc (dcc-connections connection))
    (when (close-on-main dcc)
      (quit dcc "Main IRC server connection lost.")))
  (unwind-protect
      (send-irc-message connection :quit message)
    #+(and sbcl (not sb-thread))
    (sb-sys:invalidate-descriptor (sb-sys:fd-stream-fd
                                   (network-stream connection)))
    (close (network-stream connection))))

(defmethod squit ((connection connection) (server string) (comment string))
  (send-irc-message connection :squit server comment))

(defmethod join ((connection connection) (channel string) &key password)
  (apply #'send-irc-message
         connection :join channel (when password (list password))))

(defmethod join ((connection connection) (channel channel) &key password)
  (join connection (name channel) :password password))

;; utility function not part of the RFC
(defmethod multi-join ((connection connection) (channels list))
  (dolist (channel channels)
    (join connection channel)))

(defmethod part ((connection connection) (channel string) &optional reason)
  (apply #'send-irc-message
         connection :part channel (when reason (list reason))))

(defmethod part ((connection connection) (channel channel) &optional reason)
  (part connection (name channel) reason))

;; utility function not part of the RFC
(defmethod part-all ((connection connection) &optional reason)
  (maphash #'(lambda (chan obj)
               (declare (ignore obj))
               (part connection chan reason))
           (channels connection)))

(defmethod topic- ((connection connection) (channel string) (topic string))
  (send-irc-message connection :topic channel topic))

(defmethod topic- ((connection connection) (channel channel) (topic string))
  (topic- connection (name channel) topic))

(defmethod names ((connection connection) (channel string)
                  &optional (target ""))
  (send-irc-message connection :names channel target))

(defmethod names ((connection connection) (channel channel)
                  &optional (target ""))
  (names connection (name channel) target))

(defmethod list- ((connection connection) &optional
                  (channel "") (target ""))
  (send-irc-message connection :list channel target))

(defmethod invite ((connection connection) (nickname string) (channel string))
  (send-irc-message connection :invite nickname channel))

(defmethod invite ((connection connection) (user user) (channel channel))
  (invite connection (nickname user) (name channel)))

(defmethod kick ((connection connection) (channel string)
                 (user string) &optional (comment ""))
  (send-irc-message connection :kick channel user comment))

(defmethod kick ((connection connection) (channel channel)
                 (user user) &optional (comment ""))
  (kick connection (name channel) (nickname user) comment))

(defmethod privmsg ((connection connection) (target string) (message string))
  (send-irc-message connection :privmsg target message))

(defmethod privmsg ((connection connection) (user user) (message string))
  (privmsg connection (nickname user) message))

(defmethod privmsg ((connection connection) (channel channel) (message string))
  (privmsg connection (name channel) message))

(defmethod privmsg ((connection dcc-chat-connection) target message)
  (declare (ignore target))
  (send-dcc-message connection message))

(defmethod notice ((connection connection) (target string) (message string))
  (send-irc-message connection :notice target message))

(defmethod notice ((connection connection) (user user) (message string))
  (notice connection (nickname user) message))

(defmethod notice ((connection connection) (channel channel) (message string))
  (notice connection (name channel) message))

(defmethod motd- ((connection connection) &optional (target ""))
  (send-irc-message connection :motd target))

(defmethod lusers ((connection connection) &optional (mask "") (target ""))
  (send-irc-message connection :lusers mask target))

(defmethod version ((connection connection) &optional (target ""))
  (send-irc-message connection :version target))

(defmethod stats ((connection connection) &optional (query "") (target ""))
  (send-irc-message connection :stats query target))

(defmethod links ((connection connection) &optional (remote-server "")
                  (server-mask ""))
  (send-irc-message connection :links remote-server server-mask))

(defmethod time- ((connection connection) &optional (target ""))
  (send-irc-message connection :time target))

(defun connect (&key (nickname *default-nickname*)
                     (username nil)
                     (realname nil)
                     (password nil)
                     (mode 0)
                     (server *default-irc-server*)
                     (port :default)
                     (connection-type 'connection)
                     (connection-security :none)
                     (logging-stream t))
  "Connect to server and return a connection object.

`port' and `connection-security' have a relation: when `port' equals
`:default' `*default-irc-server-port*' is used to find which port to
connect to.  `connection-security' determines which port number is found.

`connection-security' can be either `:none' or `:ssl'.  When passing
`:ssl', the cl+ssl library must have been loaded by the caller.
"
  (let* ((port (if (eq port :default)
                   ;; get the default port for this type of connection
                   (getf *default-irc-server-port* connection-security)
                 port))
         (socket (usocket:socket-connect server port
                                         :element-type 'flexi-streams:octet))
         (stream (if (eq connection-security :ssl)
                     (dynfound-funcall (make-ssl-client-stream :cl+ssl)
                                       (usocket:socket-stream socket))
                   (usocket:socket-stream socket)))
         (connection (make-connection :connection-type connection-type
                                      :network-stream stream
                                      :client-stream logging-stream
                                      :server-name server)))
    #+sbcl (setf (sb-bsd-sockets::sockopt-keep-alive (usocket:socket socket)) t)
    (unless (null password)
      (pass connection password))
    (nick connection nickname)
    (user- connection (or username nickname) mode (or realname nickname))
    (add-default-hooks connection)
    connection))

(defmethod trace- ((connection connection) &optional (target ""))
  (send-irc-message connection :trace target))

(defmethod admin ((connection connection) &optional (target ""))
  (send-irc-message connection :admin target))

(defmethod info ((connection connection) &optional (target ""))
  (send-irc-message connection :info target))

(defmethod servlist ((connection connection) &optional (mask "") (type ""))
  (send-irc-message connection :servlist mask type))

(defmethod squery ((connection connection) (service-name string) (text string))
  (send-irc-message connection :squery text service-name))

(defmethod who ((connection connection) &optional (mask "") (o ""))
  (send-irc-message connection :who mask o))

(defmethod whois ((connection connection) (mask string) &optional (target ""))
  (send-irc-message connection :whois target mask))

(defmethod whowas ((connection connection) (nickname string)
                   &optional (count "") (target ""))
  (send-irc-message connection :whowas nickname count target))

(defmethod kill ((connection connection) (nickname string) &optional (comment ""))
  (send-irc-message connection :kill comment nickname))

(defmethod kill ((connection connection) (user user) &optional (comment ""))
  (kill connection (nickname user) comment))

(defmethod ping ((connection connection) (server string))
  (send-irc-message connection :ping server))

(defmethod pong ((connection connection) (server string) &optional server2)
  (if server2
      (send-irc-message connection :pong server server2)
      (send-irc-message connection :pong server)))

(defmethod error- ((connection connection) (message string))
  (send-irc-message connection :error message))

(defmethod away ((connection connection) (message string))
  (send-irc-message connection :away message))

(defmethod rehash ((connection connection))
  (send-irc-message connection :rehash))

(defmethod die ((connection connection))
  (send-irc-message connection :die))

(defmethod restart- ((connection connection))
  (send-irc-message connection :restart))

(defmethod summon ((connection connection) (nickname string)
                   &optional (target "") (channel ""))
  (send-irc-message connection :summon nickname target channel))

(defmethod users- ((connection connection) &optional (target ""))
  (send-irc-message connection :users target))

(defmethod wallops ((connection connection) (message string))
  (send-irc-message connection :wallops message))

(defmethod userhost ((connection connection) (nickname string))
  (send-irc-message connection :userhost nickname))

(defmethod userhost ((connection connection) (user user))
  (userhost connection (nickname user)))

(defmethod ison ((connection connection) (nickname string))
  (send-irc-message connection :ison nickname))

(defmethod ison ((connection connection) (user user))
  (ison connection (nickname user)))

;; utility functions not part of the RFC
(defmethod ctcp ((connection connection) target message)
  (send-irc-message connection :privmsg target (make-ctcp-message message)))

(defmethod ctcp-reply ((connection connection) target message)
  (send-irc-message connection :notice target (make-ctcp-message message)))

(defmethod action ((connection connection) (target string) (message string))
  (ctcp connection target (concatenate 'string "ACTION " message)))

(defmethod action ((connection connection) (user user) (message string))
  (action connection (nickname user) message))

(defmethod action ((connection connection) (channel channel) (message string))
  (action connection (name channel) message))


;; Intermezzo: Manage outstanding offers

(defvar *passive-offer-sequence-token* 0)

(defgeneric dcc-add-offer (connection nickname type token &optional proto)
  (:documentation "Adds an offer to the list off outstanding offers list
for `connection'."))

(defgeneric dcc-remove-offer (connection token)
  ;; Tokens are uniquely defined within the scope of the library,
  ;; so we don't need anything but the token to actually remove an offer
  (:documentation "Remove an offer from the list of outstanding offers
for `connection'."))

(defgeneric dcc-get-offer (connection token))
(defgeneric dcc-get-offers (connection nickname &key type token))

(defun matches-offer-by-token-p (offer token)
  (equal (third offer) token))

(defun matches-offer-by-user-p (offer user)
  (equal (first offer) user))

(defun offer-matches-message-p (offer message-nick message-type message-token)
  (and (equal (first offer) message-nick)
       (equal (second offer) message-type)
       (equal (third offer) message-token)))

(defmethod dcc-add-offer (connection nickname type token &optional proto)
  (push (list nickname type token) (dcc-offers connection)))

(defmethod dcc-remove-offer (connection token)
  (setf (dcc-offers connection)
        (remove-if #'(lambda (x)
                       (matches-offer-by-token-p x token))
                   (dcc-offers connection))))

(defmethod dcc-get-offer (connection token)
  (let ((offer-list (remove-if #'(lambda (x)
                                   (not (equal (third x) token)))
                               (dcc-offers connection))))
    (first offer-list)))

(defmethod dcc-get-offers (connection nickname &key type token)
  (let* ((results (remove-if #'(lambda (x)
                                 (not (matches-offer-by-user-p x nickname)))
                             (dcc-offers connection)))
         (results (if type
                      (remove-if #'(lambda (x)
                                     (not (equal type (second x)))) results)
                    results))
         (results (if token
                      (remove-if #'(lambda (x)
                                     (not (equal token (third x)))) results))))
    results))

;; End of intermezzo

;;
;; And we move on with the definitions required to manage the protocol
;;

(defmethod ctcp-chat-initiate ((connection connection) (nickname string)
                               &key passive)
  (if passive
      ;; do passive request
      (let ((token (princ-to-string (incf *passive-offer-sequence-token*))))
        ;; tokens have been specified to be integer values,
        (dcc-add-offer connection nickname "CHAT" token)
        (ctcp connection nickname
              (format nil "DCC CHAT CHAT ~A 0 ~A"
                      (usocket:host-byte-order #(1 1 1 1))
                      token))
        token)
    ;; or do active request
    (error "Active DCC initiating not (yet) supported.")))

(defmethod ctcp-chat-initiate ((connection dcc-chat-connection)
                               nickname &key passive)
  (declare (ignore nickname passive))
  (error "Chat connection already in progress"))

(defmethod dcc-request-cancel ((connection connection) token)
  (dcc-remove-offer connection token)
  (if (stringp token)
      (let ((offer (dcc-get-offer connection token)))
        ;; We have a passive request; active ones have an associated
        ;; socket instead...
        (ctcp-reply connection (first offer)
                    (format nil "DCC REJECT ~A ~A" (second offer) token)))
    (progn
      ;; do something to close the socket here...
      ;; OTOH, we don't support active sockets (yet), so, comment out.
#|
      (usocket:socket-close token)
      (ctcp-reply connection nickname (format nil
      "ERRMSG DCC ~A timed out" type))
|#
      )))

(defmethod dcc-request-cancel ((connection dcc-chat-connection) token)
  (dcc-request-cancel (irc-connection connection) token))

(defmethod dcc-request-accept ((message ctcp-dcc-chat-request-message))
  ;; There are 2 options here: it was an active dcc offer or a passive one
  ;; For now, we'll support only active offers (where we act as a client)
  (let* ((raw-offer (car (last (arguments message))))
         (clean-offer (string-trim (list +soh+) raw-offer))
         (args (tokenize-string clean-offer))
         (remote-ip (ignore-errors (parse-integer (fourth args))))
         (remote-port (ignore-errors (parse-integer (fifth args))))
         (their-token (sixth args))
         (irc-connection (connection message)))
    (when (string= (string-upcase (third args)) "CHAT")
      (if (= remote-port 0)
          ;; a passive chat request, which we don't support (yet):
          ;; we don't act as a server yet
          (ctcp-reply irc-connection (source message)
                      "ERRMSG DCC CHAT passive-CHAT unavailable")
        (progn
          (when their-token
            (let ((offer (dcc-get-offer irc-connection their-token)))
              (when (or (null offer)
                        (not (offer-matches-message-p offer
                                                      (source message)
                                                      "CHAT" their-token)))
                (ctcp-reply irc-connection (source message)
                            (format nil
                                    "ERRMSG DCC CHAT invalid token (~A)"
                                    their-token))
                (return-from dcc-request-accept))))
          ;; ok, so either there was no token, or it matches
          ;;
          ;; When there was no token, but there was a chat request
          ;; with the same nick and type, maybe we achieved the same
          ;; in the end. (This would be caused by the other side
          ;; initiating the request manually after the client blocked
          ;; and automatic response.
          (let ((offers (dcc-get-offers irc-connection (source message)
                                        :type "CHAT")))
            (when offers
              ;; if there are more offers, consider the first fulfilled.
              (dcc-remove-offer irc-connection (third (first offers)))))

          (let ((socket (unless (or (null remote-ip)
                                    (null remote-port)
                                    (= 0 remote-port))
                   (usocket:socket-connect
                    remote-ip remote-port
                    :element-type 'flexi-streams:octet))))
            (dcc-remove-offer irc-connection their-token)
            (make-dcc-chat-connection
             :irc-connection irc-connection
             :remote-user (find-user irc-connection (source message))
             :network-stream (usocket:socket-stream socket))))))))

(defmethod dcc-request-accept ((message dcc-ctcp-dcc-chat-request-message))
  (error "DCC Chat already in progress"))

(defmethod dcc-request-reject ((message ctcp-dcc-chat-request-message)
                               &optional reason)
  (ctcp-reply (connection message) (source message)
              (format nil "ERRMSG DCC CHAT ~A" (if reason reason
                                                 "rejected"))))

(defmethod dcc-request-reject ((message dcc-ctcp-dcc-chat-request-message)
                               &optional reason)
  (ctcp-reply (irc-connection (connection message))
              (nickname (user (connection message)))
              (format nil "ERRMSG DCC CHAT ~A" (if reason reason
                                                 "rejected"))))

;;
;; IRC commands which make some sence in a DCC CHAT context
;;

(defmethod quit ((connection dcc-chat-connection)
                 &optional message)
  (when message
    (ignore-errors (send-dcc-message connection message)))
  (ignore-errors
    (dcc-close connection)))

;;## TODO
;; ctcp action, time, source, finger, ping+pong message generation
;; btw: those could be defined for 'normal' IRC too; currently
;; we only generate the responses to others' messages.
