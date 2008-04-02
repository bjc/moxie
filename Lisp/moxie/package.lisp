(defpackage moxie
  (:use :cl :cl-user)
  (:export *moxie-repl-stream*
           add-hook remove-hook run-hook
           add-keyword remove-keyword
           add-keystroke-macro remove-keystroke-macro
           map-variables escape-mux-string

           *world* world-var
           
           make-attributed-string make-attributes make-range make-font
           make-color make-super make-underline make-link
           send-to-mux write-array-to-mux print-to-world set-status-buffer clear-screen
           enable-logging disable-logging))
(in-package :moxie)

(defvar *moxie-result-stream* nil
  "Where output from the TPL goes.")

(defvar *world* nil
  "The world currently calling into a plug in function.")