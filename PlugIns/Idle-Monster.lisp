(defpackage idle-monster
  (:use :cl :cl-user :moxie :bjc-utils))
(in-package :idle-monster)

(defun send-idle-cmd (&rest args)
  (declare (ignore args))
  "Sends a command to the current world when idle."
  (print-to-world *world* (format nil "Idle!~%")))

;(add-hook 'send-idle-cmd :timer-hook)