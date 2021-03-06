;;; Keystroke macros to use the keypad for directional movement.
;; $Id: Numpad-Movement.lisp 20 2005-12-27 15:21:23Z bjc $
(defpackage :numpad-movement
  (:use :cl :cl-user :moxie :bjc-utils))
(in-package :numpad-movement)

(defun keystroke-north (&rest keywords)
  (send-to-mux *world* (format nil "north~%")))
(defun keystroke-south (&rest keywords)
  (send-to-mux *world* (format nil "south~%")))
(defun keystroke-east (&rest keywords)
  (send-to-mux *world* (format nil "east~%")))
(defun keystroke-west (&rest keywords)
  (send-to-mux *world* (format nil "west~%")))
(defun keystroke-northeast (&rest keywords)
  (send-to-mux *world* (format nil "northeast~%")))
(defun keystroke-northwest (&rest keywords)
  (send-to-mux *world* (format nil "northwest~%")))
(defun keystroke-southeast (&rest keywords)
  (send-to-mux *world* (format nil "southeast~%")))
(defun keystroke-southwest (&rest keywords)
  (send-to-mux *world* (format nil "southwest~%")))
(defun keystroke-up (&rest keywords)
  (send-to-mux *world* (format nil "up~%")))
(defun keystroke-down (&rest keywords)
  (send-to-mux *world* (format nil "down~%")))

(defun clear-screen-fun (&optional args)
  (declare (ignore arg))
  (format t "clear-screen ~S~%" *world*)
  (clear-screen *world*)
  nil)

(add-keystroke-macro 'keystroke-north :numpad-8)
(add-keystroke-macro 'keystroke-south :numpad-2)
(add-keystroke-macro 'keystroke-east :numpad-6)
(add-keystroke-macro 'keystroke-west :numpad-4)
(add-keystroke-macro 'keystroke-northeast :numpad-9)
(add-keystroke-macro 'keystroke-northwest :numpad-7)
(add-keystroke-macro 'keystroke-southeast :numpad-3)
(add-keystroke-macro 'keystroke-southwest :numpad-1)
(add-keystroke-macro 'keystroke-up :numpad-+)
(add-keystroke-macro 'keystroke-down :numpad--)
(add-keystroke-macro 'clear-screen-fun :clear)
(add-keyword 'clear-screen-fun "clear")