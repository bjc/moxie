;;; -*- Lisp -*-
;; $Id: compat-clisp.lisp 40 2006-01-02 03:35:07Z bjc $
(in-package :moxie)

(defun make-result-stream ()
  (ext:make-stream 3 :direction :output))

(defun coerce-inet-address-designator (host)
  "Coerce HOST into an addess vector.")

(defun open-connection (host port &key (buffering :full))
  "Opens a connection to HOST:PORT, returning a STREAM if successful, NIL otherwise.")

(defun close-connection (stream)
  "Closes STREAM.")

(defun add-input-handler (stream handler)
  "Adds HANDLER to the input handler list on SOCKET.")

(defun remove-input-handler (handler))

(defun save-lisp-and-die (path)
  (ext:saveinitmem path)
  (ext:quit))
