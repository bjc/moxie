;; -*- Lisp -*-
;; $Id: Telnet.lisp 20 2005-12-27 15:21:23Z bjc $

(defpackage telnet-options
  (:use :cl :cl-user :moxie :bjc-utils))
(in-package :telnet-options)

(defconstant +iac+ 255
  "Interpret as Command")

(defconstant +se+ 240
  "End of subnegotiation parameters.")

(defconstant +nop+ 241
  "No operation.")

(defconstant +data-mark+ 242
  "The data stream portion of a Synch.
This should always be accompanied
by a TCP Urgent notification.")

(defconstant +break+ 243
  "NVT character BRK.")

(defconstant +interrupt-process+ 244
  "The function IP.")

(defconstant +abort-output+ 245
  "The function AO.")

(defconstant +are-you-there+ 246
  "The function AYT.")

(defconstant +erase-character+ 247
  "The function EC.")

(defconstant +erase-line+ 248
  "The function EL.")

(defconstant +go-ahead+ 249
  "The GA signal.")

(defconstant +sb+ 250
  "Indicates that what follows is
subnegotiation of the indicated option.")

(defconstant +will+ 251
  "Indicates the desire to begin
performing, or confirmation that
you are now performing, the
indicated option.")

(defconstant +wont+ 252
  "Indicates the refusal to perform,
or continue performing, the
indicated option.")

(defconstant +do+ 253
  "Indicates the request that the
other party perform, or
confirmation that you are expecting
the other party to perform, the
indicated option.")

(defconstant +dont+ 254
  "Indicates the demand tha the
other party stop performing,
or confirmation that you are no
longer expecting the other party
to perform, the indicated option.")

(defconstant +option-binary+ 0
  "Enable binary (8 bit) data transmission, instead of the stripped 7 bit ASCII default.")
(defconstant +option-echo+ 1
  "Enable remote echo, suppressing local echo.")
(defconstant +option-supress-go-ahead+ 3
  "Enable go-ahead suppression.")
(defconstant +option-status+ 5
  "Enable option spamming for easier negotiations.")
(defconstant +option-timing-mark+ 6
  "Return a timing mark when this is recieved.")
(defconstant +option-terminal+ 24
  "Return terminal type.")
(defconstant +option-window-size+ 31
  "Negotiate about window size.")
(defconstant +option-authentication+ 37
  "Negotiate authentication.")
(defconstant +option-environment+ 39
  "Negotiate environment variables.")
(defconstant +option-extended-options-list+ 255
  "Read the next byte for further options.")

(defconstant +option-mccp1+ 85
  "Mud Client Compression Protocol version 1.")
(defconstant +option-mccp2+ 86
  "Mud Client Compression Protocol version 2.")
(defconstant +option-msp+ 90
  "Mud Sound Protocol.")
(defconstant +option-mxp+ 91
  "Mud eXtension Protocol.")

(defun ack-cmd (cmd)
  "Computes the ACK code for CMD."
  (cond ((eql cmd +do+) +will+)
        ((eql cmd +will+) +do+)
        ((eql cmd +dont+) +wont+)
        ((eql cmd +wont+) +dont+)))

(defun nack-cmd (cmd)
  "Computes the NACK code for CMD."
  (cond ((eql cmd +do+) +wont+)
        ((eql cmd +will+) +dont+)
        ((eql cmd +dont+) +will+)
        ((eql cmd +wont+) +do+)))

(defun send-option (cmd option)
  (let ((options (world-var 'options)))
    (unless options
      (setf (world-var 'options) (make-hash-table))
      (setf options (world-var 'options)))
    (unless (eql cmd (gethash option options))
      (setf (gethash option options) cmd)
      (send-bytes (list +iac+ cmd option)))))

(defun send-bytes (bytes)
  (write-array-to-mux *world* bytes))

(defun handle-command-array (array)
  "Array is a byte-vector of the complete IAC code, including any IAC characters."
  (format t "(handle-command-array ~S)~%" array)
  (when (and (> (length array) 1) (eql (elt array 0) +iac+))
    (let ((cmd (elt array 1)))
      (cond ((or (eql cmd +do+) (eql cmd +will+))
             (awhen (aand (> (length array) 2) (elt array 2))
               (cond ((eql it +option-timing-mark+)
                      (send-bytes (list +iac+ ack-cmd cmd) it))
                     ((or (eql it +option-binary+)
                          (eql it +option-supress-go-ahead+)
                          (eql it +option-status+)
                          (eql it +option-msp+)
                          (eql it +option-mxp+))
                      (send-option (ack-cmd cmd) it))
                     (t (send-option (nack-cmd cmd) it)))))
            ((or (eql cmd +dont+) (eql cmd +wont+))
             (awhen (aand (> (length array) 2) (elt array 2))
               (send-option (ack-cmd cmd) it)))
            (t (format t "Can't handle command ~S.~%" cmd))))))

(add-hook 'handle-command-array :telnet-option-hook)
