;;; -*- Lisp -*-
;; $Id: moxie.asd,v 1.1.1.1 2005/02/15 06:06:59 shmit Exp $
#|
Hooks:

;; Sent from world handlers
:world-opened-hook *world*
:world-closed-hook *world*
:input-from-client-hook line
:output-from-server-hook line

;; This can probably just be a plugin, off :output-from-server-hook
:telnet-option-hook telnetCodes

;; Controlled by the front end, ultimately.
:start-logging-hook
:stop-logging-hook

;; XXX: IDK
:timer-hook
|#