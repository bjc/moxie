;;; -*- Lisp -*-
;;; $Id: Logger.lisp 20 2005-12-27 15:21:23Z bjc $

(defpackage default-logger
  (:use :cl :cl-user :moxie :bjc-utils))
(in-package :default-logger)

(defvar *default-log-directory* (merge-pathnames "Documents/Moxie Transcripts/"
                                                 (user-homedir-pathname)))

(defun start-logging-hook (&optional arg)
  (let ((log-filen (car (world-var :log-file-path))))
    (setf (world-var :log-stream)
          (open log-filen :direction :output
                :if-exists :append :if-does-not-exist :create))
    (awhen (world-var :log-stream)
      (format it "[Logging started on: ~A]~%" (format-timestamp (get-universal-time)))
      (finish-output it)
      (print-to-world *world* (format nil "Now logging to: ~A~%" log-filen)))))

(defun stop-logging-hook (&optional arg)
  (awhen (world-var :log-stream)
    (format it "[Logging ended on: ~A]~%" (format-timestamp (get-universal-time)))
    (finish-output it)
    (close it)
    (setf (world-var :log-stream) nil)
    (print-to-world *world* (format nil "Logging is now disabled.~%"))))

(defun log-output (string)
  "Log STRING to DEFAULT-FILE-NAME."
  (awhen (world-var :log-stream)
    (let* (*print-pretty*
           (string (if (stringp string) string (car string))))
      (format it "~A ~A" (format-timestamp (get-universal-time)) string)
      (finish-output it))))

(defun log-input (string)
  "Log STRING to DEFAULT-FILE-NAME."
  (awhen (world-var :log-stream)
    (let* (*print-pretty*
           (string (if (stringp string) string (car string))))
      (format it "~A -> ~A~%" (format-timestamp (get-universal-time)) string)
      (finish-output it))))

(defun format-timestamp (universal-time)
  (multiple-value-bind (sec min hour date mon year day daylight-p zone)
      (decode-universal-time universal-time)
    (format nil "[~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D]" year mon date hour min sec)))

;;
;; Convienence aliases
;;
(defun logger-alias (arg)
  (if (> (length arg) 0)
      (start-logging-to arg)
      (if (world-var :log-stream)
          (disable-logging *world*)
          (enable-logging *world*)))
  nil)

(defun start-logging-to (filename)
  (awhen (world-var :log-stream)
    (disable-logging *world*))
  (setf (world-var :log-file-path)
        (list (merge-pathnames filename *default-log-directory*)))
  (enable-logging *world*))

(add-hook 'start-logging-hook :start-logging-hook)
(add-hook 'stop-logging-hook :stop-logging-hook)
(add-hook 'log-output :output-from-server-hook)
(add-hook 'log-input :input-from-client-hook)

(add-keyword 'logger-alias "log")