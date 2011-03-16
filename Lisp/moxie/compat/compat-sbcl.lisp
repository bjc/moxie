;;; -*- Lisp -*-
;; $Id: compat-sbcl.lisp 36 2006-01-01 20:47:40Z bjc $
(in-package :moxie)

(defvar *stream-to-handler* (make-hash-table))
(defvar *stream-to-socket* (make-hash-table))

(defun make-result-stream ()
  (sb-sys:make-fd-stream 3 :output t))

(defun coerce-inet-address-designator (host)
  "Coerce HOST into an addess vector."
  (cond ((typep host '(vector (unsigned-byte 8) 4)) host)
        ((some #'alpha-char-p host) (sb-bsd-sockets:host-ent-address
                                     (sb-bsd-sockets:get-host-by-name host)))
        (t (sb-bsd-sockets:make-inet-address host))))

(defun open-connection (host port &key (buffering :full))
  "Opens a connection to HOST:PORT, returning a STREAM if successful, NIL otherwise."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream
                               :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket (coerce-inet-address-designator host) port)
    (let ((stream (sb-bsd-sockets:socket-make-stream socket
                                                     :input t :output t :buffering buffering :external-format :latin-1)))
      (setf (gethash stream *stream-to-socket*) socket)
      stream)))

(defun close-connection (stream)
  "Closes STREAM."
  (ignore-errors
    (remove-input-handler stream)
    (remhash stream *stream-to-socket*)
    (close stream)))

(defun add-input-handler (stream handler)
  "Adds HANDLER to the input handler list on SOCKET."
  (setf (gethash stream *stream-to-handler*)
        (sb-sys:add-fd-handler (sb-bsd-sockets:socket-file-descriptor (gethash stream *stream-to-socket*))
                               :input
                               (lambda (fd)
                                 (declare (ignore fd))
                                 (funcall handler stream)))))

(defun remove-input-handler (stream)
  (awhen (gethash stream *stream-to-handler*)
    (sb-sys:remove-fd-handler it)))

(defun save-lisp-and-die (path)
  (sb-ext:save-lisp-and-die path))
