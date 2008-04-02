;; -*- Lisp -*-
;; $Id: MXP.lisp 20 2005-12-27 15:21:23Z bjc $

(defpackage mxp
  (:use :cl :cl-user :moxie :bjc-utils))
(in-package :mxp)

(defun parse-mxp-args (string)
  (let ((last-space 0))
    (loop as next-space = (position-if (lambda (c)
                                         (or (eql c #\Space) (eql c #\Tab) (eql c #\Newline)))
                                       string
                                       :start last-space)
         collect (if next-space 
                     (prog1
                       (subseq string last-space next-space)
                       (setf last-space (1+ next-space)))
                     (subseq string last-space (length string)))
         while next-space)))

;; We have to get the world name here, which isn't being set
;; for some reason in the world-opened-hook.
;;
;; Update: the reason is that world-opened-hook doesn't have
;; anything on it. We need to create a function to make the
;; alist in args the environment for the world, and figure out
;; where to put it.
;;
;; Built in plugin via runhook?
;; Do we want runhook at all, since this is so low-level we may
;; not want people messing with it.
(defun play-sound (args)
  (format t "msp dir: ~S~%" (merge-pathnames (car args)
                                             (merge-pathnames "Library/Moxie/MSP/"
                                             (user-homedir-pathname)))))

(defun dispatch-mxp-command (string)
  (let* ((strlen (length string))
         (lp-pos (position #\( string))
         (rp-pos (and lp-pos (position #\) string)))
         (cmd (subseq string 0 (or lp-pos strlen)))
         (args (and rp-pos (parse-mxp-args (subseq string (1+ lp-pos) rp-pos)))))
    (cond ((or (string= cmd "MUSIC") (string= cmd "SOUND"))
           (play-sound args))
          (t (format t "Found MXP cmd: ~S, args: ~S~%" cmd args)))
    (subseq string (1+ rp-pos))))

(defun scan-mxp-data (string)
  (let ((string (if (stringp string) string (car string))))
    (when (and (> (length string) 2)
               (string= (subseq string 0 2) "!!"))
      (dispatch-mxp-command (subseq string 2)))))

(add-hook 'scan-mxp-data :output-from-server-hook)
