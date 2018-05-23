;;;; $Id: variable.lisp 245 2015-09-06 13:16:49Z ehuelsmann $
;;;; $URL: file:///project/cl-irc/svn/tags/0.9.2/variable.lisp $

;;;; See the LICENSE file for licensing information.

(in-package :irc)

(defvar *debug-p* nil)
(defvar *debug-stream* t)

(defconstant +soh+ #.(code-char 1))

(defparameter *version* "0.9.2")
(defparameter *ctcp-version*
  (format nil "CL IRC library, cl-irc:~A:~A ~A"
          *version* (machine-type) (machine-version)))

(defparameter *download-host* "http://common-lisp.net/")
(defparameter *download-directory* "/project/cl-irc/")
(defparameter *download-file*
  (format nil "cl-irc-~A.tar.gz" *version*))

(defvar *default-nickname* "cl-irc")
(defvar *default-irc-server* "irc.freenode.net")
(defvar *default-irc-server-port* '(:none 6667  ;; most used for normal IRC
                                    :ssl  6679  ;; most used for SSL IRC
                                    ))
(defvar *default-quit-message*
  "Common Lisp IRC library - http://common-lisp.net/project/cl-irc")

(defparameter *unknown-reply-hook* nil
  "A function of two arguments, called with the related irc connection
object and the protocol message string upon detection of an unmappable
response code.

The function should return a valid IRC-MESSAGE class or NIL.

The parameter can be NIL to disable the hook.")

(defparameter *default-isupport-CHANMODES*
  "beI,kO,l,aimnpqsrt")
(defparameter *default-isupport-PREFIX*
  "(ov)@+")

(defparameter *default-isupport-values*
  `(("CASEMAPPING" "rfc1459")
    ("CHANMODES" ,*default-isupport-CHANMODES*)
    ("CHANNELLEN" "200")
    ("CHANTYPES" "#&")
    ("MODES" "3")
    ("NICKLEN" "9")
    ("PREFIX" ,*default-isupport-PREFIX*)
    ("TARGMAX")))

(defparameter *default-outgoing-external-format* '(:utf-8)
  "The external-format we use to encode outgoing messages. This
  should be an external format spec that flexi-streams accepts.

  :eol-style will always be overridden to be :crlf as required
  by the IRC protocol.")

(defparameter *default-incoming-external-formats* '((:utf-8 :eol-style :crlf)
                                                    (:latin1 :eol-style :crlf))
  "The external-formats we use to decode incoming messages. This should
  be a list of external format specs that flexi-streams accepts.

  The external formats are tried in order, until one decodes the
  message without encoding errors. Note that the last external
  format should be a single-byte one with most or even all valid
  codepoints (such as latin-1).

  :eol-style will always be overridden to be :crlf as required by the
  IRC protocol.")

(defvar *dcc-connections* nil)

(defstruct (mode-description (:conc-name "MODE-DESC-"))
  (char nil)
  (symbol nil)
  (param-on-set-p nil)
  (param-on-unset-p nil)
  (nick-param-p nil)
  (class 'single-value-mode))


(defparameter *default-char-to-channel-modes-map*
  '(
    ;; these modes don't take parameters
    (#\a . :anonymous)
    (#\i . :invite-only)
    (#\m . :moderated)
    (#\n . :no-external)
    (#\q . :quiet)
    (#\s . :secret)
    (#\r . :reop)
    (#\t . :op-only-topic)

    ;; these modes take a user parameter
    (#\O . :channel-creator)
    (#\o . :channel-operator)
    (#\v . :voice)

    ;; these modes take a parameter other than a user
    (#\l . :limit)
    (#\k . :key)
    (#\b . :ban)
    (#\e . :except)
    (#\I . :invite)))

(defparameter *char-to-user-modes-map*
  '((#\a . :away)
    (#\i . :invisible)
    (#\w . :receive-wallops)
    (#\s . :server-notices)
    (#\r . :restricted-connection)
    (#\o . :remote-operator)
    (#\O . :local-operator)))

(defparameter *reply-names*
  '((1 :rpl_welcome)
    (2 :rpl_yourhost)
    (3 :rpl_created)
    (4 :rpl_myinfo)
    (5 :rpl_isupport) ;; The RFC was wrong to define RPL_BOUNCE here,
     ;; see http://www.irc.org/tech_docs/draft-brocklesby-irc-isupport-03.txt
    (10 :rpl_bounce)
    (15 :rpl_map) ; From ircd 2.11 source
    (17 :rpl_mapend) ; From ircd 2.11 source
    (18 :rpl_mapstart) ; From ircd 2.11 source
    (20 :rpl_hello) ; From ircd 2.11 source
    (42 :rpl_yourid) ; From ircd 2.11 source
    (43 :rpl_savenick) ; From ircd 2.11 source
    (200 :rpl_tracelink)
    (201 :rpl_traceconnecting)
    (202 :rpl_tracehandshake)
    (203 :rpl_traceunknown)
    (204 :rpl_traceoperator)
    (205 :rpl_traceuser)
    (206 :rpl_traceserver)
    (207 :rpl_traceservice)
    (208 :rpl_tracenewtype)
    (209 :rpl_traceclass)
    (210 :rpl_tracereconnect)
    (211 :rpl_statslinkinfo)
    (212 :rpl_statscommands)
    (213 :rpl_statscline)
    (214 :rpl_statsnline)
    (215 :rpl_statsiline)
    (216 :rpl_statskline)
    (217 :rpl_statsqline)
    (218 :rpl_statsyline)
    (219 :rpl_endofstats)
    (221 :rpl_umodeis)
    (225 :rpl_statsdline) ; Seen in dancer ircd source
    (227 :rpl_option) ; Seen in dancer ircd source
    (228 :rpl_endoptions) ; Seen in dancer ircd source
    (231 :rpl_serviceinfo)
    (232 :rpl_endofservices)
    (233 :rpl_service)
    (234 :rpl_servlist)
    (235 :rpl_servlistend)
    (240 :rpl_statsvline)
    (241 :rpl_statslline)
    (242 :rpl_statsuptime)
    (243 :rpl_statsonline)
    (244 :rpl_statshline)
    (245 :rpl_statssline) ; The RFC says 244 but I believe that was a typo.
    (246 :rpl_statsping)
    (247 :rpl_statsbline)
    (248 :rpl_statsuline) ; Seen in dancer ircd source
    (249 :rpl_statsdebug) ; Seen in dancer ircd source
    (250 :rpl_statsdline)
    (251 :rpl_luserclient)
    (252 :rpl_luserop)
    (253 :rpl_luserunknown)
    (254 :rpl_luserchannels)
    (255 :rpl_luserme)
    (256 :rpl_adminme)
    (257 :rpl_adminloc1)
    (258 :rpl_adminloc2)
    (259 :rpl_adminemail)
    (261 :rpl_tracelog)
    (262 :rpl_traceend)
    (263 :rpl_tryagain)
    (265 :rpl_localusers) ; Seen in dancer ircd source
    (266 :rpl_globalusers) ; Seen in dancer ircd source
    (268 :rpl_mode) ; Seen in dancer ircd source
    (269 :rpl_endmode) ; Seen in dancer ircd source
    (271 :rpl_sitelist) ; Seen in dancer ircd source
    (272 :rpl_endsitelist) ; Seen in dancer ircd source
    (290 :rpl_clientcapab) ; Seen in dancer ircd source
    (292 :rpl_noservicehost)
    (300 :rpl_none)
    (301 :rpl_away)
    (302 :rpl_userhost)
    (303 :rpl_ison)
    (304 :rpl_away)
    (305 :rpl_unaway)
    (306 :rpl_noaway)
    (307 :rpl_whoisidentified)
    (311 :rpl_whoisuser)
    (312 :rpl_whoisserver)
    (313 :rpl_whoisoperator)
    (314 :rpl_whowasuser)
    (315 :rpl_endofwho)
    (316 :rpl_whoischanop)
    (317 :rpl_whoisidle)
    (318 :rpl_endofwhois)
    (319 :rpl_whoischannels)
    (320 :rpl_whoisidentified) ; Seen in dancer ircd source
    (321 :rpl_liststart)
    (322 :rpl_list)
    (323 :rpl_listend)
    (324 :rpl_channelmodeis)
    (325 :rpl_uniqopis)
    (326 :rpl_whoisoperprivs) ; Seen in dancer ircd source
    (327 :rpl_whoisrealhost) ; Seen in dancer ircd source
    (328 :rpl_channel_url)
    (329 :rpl_creationtime) ; Seen in dancer ircd source
    (330 :rpl_whoisidentified)
    (331 :rpl_notopic)
    (332 :rpl_topic)
    (333 :rpl_topicwhotime) ; Seen in dancer ircd source
    (341 :rpl_inviting)
    (342 :rpl_summoning)
    (346 :rpl_invitelist)
    (347 :rpl_endofinvitelist)
    (348 :rpl_exceptlist)
    (349 :rpl_endofexceptlist)
    (351 :rpl_version)
    (352 :rpl_whoreply)
    (353 :rpl_namreply)
    (361 :rpl_killdone)
    (362 :rpl_closing)
    (363 :rpl_closeend)
    (366 :rpl_endofnames)
    (364 :rpl_links)
    (365 :rpl_endoflinks)
    (367 :rpl_banlist)
    (368 :rpl_endofbanlist)
    (369 :rpl_endofwhowas)
    (371 :rpl_info)
    (372 :rpl_motd)
    (373 :rpl_infostart)
    (374 :rpl_endofinfo)
    (375 :rpl_motdstart)
    (376 :rpl_endofmotd)
    (377 :rpl_map) ; Seen in dancer ircd source
    (378 :rpl_endofmap) ; Seen in dancer ircd source
    (379 :rpl_forward) ; Seen in dancer ircd source
    (381 :rpl_youreoper)
    (382 :rpl_rehashing)
    (383 :rpl_yourservice)
    (384 :rpl_myportis)
    (391 :rpl_time)
    (392 :rpl_usersstart)
    (393 :rpl_users)
    (394 :rpl_endofusers)
    (395 :rpl_nousers)
    (396 :rpl_hiddenhost)
    (399 :rpl_message) ; Seen in dancer ircd source
    (401 :err_nosuchnick)
    (402 :err_nosuchserver)
    (403 :err_nosuchchannel)
    (404 :err_cannotsendtochan)
    (405 :err_toomanychannels)
    (406 :err_wasnosuchnick)
    (407 :err_toomanytargets)
    (408 :err_nosuchservice)
    (409 :err_noorigin)
    (410 :err_services_offline) ; Seen in dancer ircd source
    (411 :err_norecipient)
    (412 :err_notexttosend)
    (413 :err_notoplevel)
    (414 :err_wildtoplevel)
    (415 :err_badmask)
    (421 :err_unknowncommand)
    (422 :err_nomotd)
    (423 :err_noadmininfo)
    (424 :err_fileerror)
    (431 :err_nonicknamegiven)
    (432 :err_erroneusnickname)
    (433 :err_nicknameinuse)
    (436 :err_nickcollision)
    (437 :err_unavailresource)
    (438 :err_bannickchange) ; Seen in dancer ircd source
    (441 :err_usernotinchannel)
    (442 :err_notonchannel)
    (443 :err_useronchannel)
    (444 :err_nologin)
    (445 :err_summondisabled)
    (446 :err_userdisabled)
    (447 :err_targetninvite) ; Seen in dancer ircd source
    (448 :err_sourceninvite) ; Seen in dancer ircd source
    (451 :err_notregistered)
    (461 :err_needmoreparams)
    (462 :err_alreadyregistered)
    (463 :err_nopermforhost)
    (464 :err_passwdmismatch)
    (465 :err_yourebannedcreep)
    (466 :err_youwillbebanned)
    (467 :err_keyset)
    (471 :err_channelisfull)
    (472 :err_unknownmode)
    (473 :err_inviteonlychan)
    (474 :err_bannedfromchan)
    (475 :err_badchannelkey)
    (476 :err_badchanmask)
    (477 :err_nochanmodes)
    (478 :err_banlistfull)
    (479 :err_badchanname) ; Seen in dancer ircd source
    (480 :err_throttled) ; Seen in dancer ircd source
    (481 :err_noprivileges)
    (482 :err_chanoprivsneeded)
    (483 :err_cantkillserver)
    (484 :err_restricted)
    (485 :err_uniqopprivsneeded)
    (486 :err_restricted) ; Seen in dancer ircd source
    (487 :err_no_op_split) ; Seen in dancer ircd source
    (488 :err_need_umode) ; Seen in dancer ircd source
    (491 :err_nooperhost)
    (501 :err_umodeunknownflag)
    (502 :err_usersdontmatch)
    (503 :err_ghostedclient) ; Seen in dancer ircd source
    (505 :err_blocking_notid) ; Seen in dancer ircd source
    (511 :err_sitelistfull) ; Seen in dancer ircd source
    (512 :err_maxmapnodes) ; Seen in dancer ircd source
    (513 :err_maxforwarding) ; Seen in dancer ircd source
    (514 :err_noforwarding) ; Seen in dancer ircd source
    (515 :err_nounidentified) ; Seen in dancer ircd source
    (516 :err_last_err_msg) ; Seen in dancer ircd source
    (671 :rpl_secureconnection)
    (710 :rpl_knock)
    ))

