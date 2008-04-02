;;; -*- Lisp -*-
;; $Id: compat-openmcl.lisp 36 2006-01-01 20:47:40Z bjc $
(in-package :moxie)

(defvar *stream-to-process* (make-hash-table))
(defvar *stream-to-handler* (make-hash-table))

(defmacro with-thread (thread &body body)
  `(ccl:process-interrupt ,thread
                          (lambda ()
                            ,@body)))

(defun make-result-stream ()
  (ccl::make-fd-stream 3 :direction :output))

(defun coerce-inet-address-designator (host)
  "Coerce HOST into an addess vector."
  (or (and (integerp host) host)
      (ccl:dotted-to-ipaddr host :errorp nil)
      (ignore-errors (ccl:lookup-hostname host))))

(defun open-connection-thread (parent stream)
  (ccl:socket-connect stream)
  (loop
     (ccl:process-input-wait (ccl:stream-device stream :input))
     (let ((handler (gethash stream *stream-to-handler*)))
       (with-thread parent
         (funcall handler stream)))))

(defun open-connection (host port &rest args)
  "Opens a connection to HOST:PORT, returning a STREAM if successful, NIL otherwise."
  (declare (ignore args))
  (let ((s (ccl:make-socket :address-family :internet :type :stream :connect :active
                            :remote-host (coerce-inet-address-designator host)
                            :remote-port port)))
    (setf (gethash s *stream-to-process*)
          (ccl:process-run-function (format nil "Connection to ~A:~A" host port)
                                    #'open-connection-thread
                                    ccl:*current-process* s))
    s))

(defun close-connection (stream)
  "Closes STREAM."
  (ignore-errors
    (close stream)
    (ccl:process-kill (gethash stream *stream-to-process*))
    (remove-input-handler stream)
    (remhash stream *stream-to-process*)))

(defun add-input-handler (stream handler)
  "Adds HANDLER to the input handler list on STREAM."
  (setf (gethash stream *stream-to-handler*) handler))

(defun remove-input-handler (stream)
  "Removes all handlers from STREAM."
  (remhash stream *stream-to-handler*))

(defun save-lisp-and-die (path)
  (ccl:save-application path))