;;; -*- Lisp -*-
;; $Id: world.lisp 20 2005-12-27 15:21:23Z bjc $
;;
;; Functions that should eventually be moved to the real plug in
;; support methodology (i.e., with nib files).
;;
(in-package :moxie)

(defun notify-front-end-load (&rest args)
  (declare (ignore args))
  (send-event-to-world *world* :change-settings (world-vars *world*)))

(defun notify-front-end-close (&rest args)
  (declare (ignore args))
  (send-event-to-world *world* :world-closed))

(defun notify-front-end-connect (&rest args)
  (declare (ignore args))
  (set-status-buffer "Connected")
  (send-event-to-world *world* :world-connected))

(defun notify-front-end-disconnect (&rest args)
  (declare (ignore args))
  (set-status-buffer "Disconnected")
  (send-event-to-world *world* :world-disconnected))

(defun notify-front-end-data (line)
  (print-to-world *world* line))

(defun notify-back-end-data (line)
  (print-to-world *world* (format nil "-> ~A~%" line)))

(defun notify-back-end-settings (alist)
  (when (world-save-path *world*)
    (format t "DEBUG: saving new settings: ~S~%" alist)
    (save-world-state *world*)))

(add-hook 'notify-front-end-load :world-loaded-hook)
(add-hook 'notify-front-end-close :world-closed-hook)
(add-hook 'notify-front-end-connect :world-connected-hook)
(add-hook 'notify-front-end-disconnect :world-disconnected-hook)
(add-hook 'notify-front-end-data :output-from-server-hook)
(add-hook 'notify-back-end-data :input-from-client-hook)
(add-hook 'notify-back-end-settings :setting-changed-hook)

(defun show-by-filter (key val &optional (test #'eql))
  (map-by-filter (lambda (world)
                   (format t "Matches ~S = ~S: ~S~%" key val world))
                 key val test))

(defun map-by-filter (fn key val &optional (test #'eql))
  (map-worlds (lambda (world)
                (when (funcall test (world-var key world) val)
                  (funcall fn world)))))

(defun do-auto-connect (&rest args)
  (declare (ignore args))
  (setf *tmp* *world*)
  (when (and (not (world-connected *world*)) (world-var :connect-on-open))
    (format t "DEBUG: auto-connecting to ~A:~A~%" (world-var :hostname) (world-var :port))
    (world-connect)))

(add-hook 'do-auto-connect :world-loaded-hook)