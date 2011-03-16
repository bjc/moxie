;;; The lisp bootstrapping code.
;; $Id: moxie.lisp 29 2005-12-31 22:59:17Z bjc $

(in-package :moxie)

(defvar *hooks* (make-hash-table)
  "The hooks.
See the functions add-hook and remove-hook.")

(defun add-hook (sym mode)
  "Adds the function SYM to the list MODE."
  (setf (gethash mode *hooks*)
        (let ((hooks (reverse (gethash mode *hooks*))))
          (pushnew sym hooks)
          (nreverse hooks))))

(defun remove-hook (sym mode)
  "Removes the function HOOK from the list MODE."
  (setf (gethash mode *hooks*) (remove sym (gethash mode *hooks*))))

;; We should see how many args there are, and pass that amount in. Not just the return
;; value. But for now, this means hooks need at least one arg.
(defun run-hook (mode &optional arg)
  "Runs all the hooks for MODE, in order of how they were attached."
  (let ((result nil))
    (do ((hooks (gethash mode *hooks*) (cdr hooks)))
        ((or (null hooks) (null (car hooks))) result)
      (awhen (funcall (car hooks) (or result arg))
        (setf result it)))))

(defvar *keywords* (make-hash-table :test #'equal))

(defun add-keyword (sym key)
  "Adds /KEY as a keyword, calling SYM with the rest of the input string."
  (setf (gethash (string-upcase key) *keywords*) sym))

(defun remove-keyword (key)
  "Removes /KEY as a keyword."
  (remhash (string-upcase key) *keywords*))

(defun get-keyword (string)
  "Finds the keyword in STRING, if any."
  (when (and (> (length string) 0) (eql #\/ (elt string 0)))
    (let ((pos (or (position-if (lambda (c)
                                  (or (eql #\Space c)
                                      (eql #\Newline c)
                                      (eql #\Tab c)))
                            string)
                   (length string))))
      (values
       (string-upcase (subseq string 1 pos))
       (aif (and (< pos (length string))
                 (position-if-not (lambda (c)
                                    (or (eql #\Space c)
                                        (eql #\Newline c)
                                        (eql #\Tab c)))
                                  string
                                  :start pos))
            (subseq string it (length string))
           "")))))

(defun run-keyword-hook (string &rest keywords)
  "Runs through the keyword database for the word at the beginning of STRING."
  (multiple-value-bind (key rem) (get-keyword string)
    (when key
      (or (aand (gethash key *keywords*) (apply it rem keywords)) ""))))

(add-hook 'run-keyword-hook :input-from-client-hook)

;; Keystrokes are keywords that look like this:
;;   keystroke := :[<modifier>-]*<keycode>
;;   modifier := cmd|opt|ctrl|shift|numpad
;;   keycode := <fkey>|character
;;   fkey := f1 .. fn .. f35
;;
;; So, CMD-NUMPAD-8 is:
;;   :cmd-numpad-8
;;
;; Okay, that won't work for the long term, because :cmd-shift-numpad-8 will be
;; evaluated differently than :shift-cmd-numpad-8.
(defvar *keystroke-macros* (make-hash-table)
  "The keystroke macro to symbol dispatch table.")

(defun add-keystroke-macro (sym keystroke)
  "Adds KEYSTROKE as a keystroke-macro, calling SYM on dispatch."
  (setf (gethash keystroke *keystroke-macros*) sym)
  (register-keystroke-macro keystroke))

(defun remove-keystroke-macro (keystroke)
  "Removes any hint of KEYSTROKE being invoked as a keystroke-macro."
  (remhash keystroke *keystroke-macros*)
  (unregister-keystroke-macro keystroke))

(defun run-keystroke-macro-hook (keystroke)
  "Dispatches KEYSTROKE to the appropriate hook function."
  (awhen (gethash keystroke *keystroke-macros*)
    (funcall it keystroke)))

(add-hook 'run-keystroke-macro-hook :keystroke-macro-hook)

;;
;; Utility functions
;;
(defun map-variables (string vars)
  "Returns a string made of of substituting $[0-9]+$ in STRING variables with those positions in VARS."
  (with-output-to-string (result)
    (let ((strlen (1- (length string))))
      (loop for i from 0 to strlen
         as char = (elt string i)
         do (aif (aand (< (1+ i) strlen) (eql char #\$)
                       (position #\$ string :start (1+ i)))
                 (let ((var (parse-integer (subseq string (1+ i) it))))
                   (when var
                     (princ (elt vars (1- var)) result))
                   (setq i it))
                 (princ char result))))
    result))

(defun escape-mux-string (string)
  "Returns a string made from STRING with substitutions for white space."
  (with-output-to-string (result)
    (let ((strlen (length string)))
      (loop for i from 0 to (1- strlen)
         as char = (elt string i)
         do (case char
              ((#\Space)
               (princ "%b" result))
              ((#\Tab)
               (princ "%t" result))
              ((#\Newline #\Return)
               (princ "%r" result))
              (t (princ char result)))))
    result))

(defun make-attributed-string (string &rest attribute-ranges)
  (list string attribute-ranges))

(defun make-attributes (&rest attributes)
  attributes)

(defun make-range (location length)
  (list :range location length))

(defun make-color (r g b)
  (list :color r g b))

(defun make-font (name size)
  (list :font name size))

(defun make-super (n)
  (cons :super n))

(defun make-underline (n)
  (cons :underline n))

(defun make-link (url)
  (cons :link url))

;;
;; Low level commands which interface directly to Moxie.
;;
;; Useful stuff to add:
;;   say, for speaking text
;;   playsound/music, for sound effects
;;

(defmacro with-response (cmd-and-args &body body)
  `(progn
     (apply #'send-command ,@cmd-and-args)
     (let ((response (read)))
       ,@body)))

(defun write-array-to-mux (world &rest args)
  "Send ARGS to the output window associated with WORLD."
  (format (world-stream world) "~S~%" args)
  (finish-output (world-stream world)))

(defun send-to-mux (world &rest args)
  "Send ARGS to the MUX associated with WORLD."
  (format (world-stream world) "~A~%" (car args))
  (finish-output (world-stream world)))

(defun print-to-world (world &rest args)
  "Send ARGS to the output window associated with WORLD."
  (apply #'send-event-to-world world :output-from-server-hook args))

(defun register-keystroke-macro (keystroke)
  "Register KEYSTROKE as a macro with Moxie."
  (send-command :register-keystroke keystroke))

(defun unregister-keystroke-macro (keystroke)
  "Unregisters KEYSTROKE as a macro with Moxie."
  (send-command :unregister-keystroke keystroke))

(defun set-status-buffer (string &optional (world *world*))
  "Set the status buffer of the window associated with WORLD to STRING."
  (send-event-to-world world :set-status-buffer string))

(defun clear-screen (world)
  (send-event-to-world world :clear-screen))

(defun enable-logging (world)
  "Enable logging for WORLD."
  (send-event-to-world world :enable-logging))

(defun disable-logging (world)
  "Disable logging for WORLD."
  (send-event-to-world world :disable-logging))

(defun send-event-to-world (world event &rest args)
  "Send EVENT and ARGS to WORLD's result handler."
  (apply #'send-command (world-id world) event args))

(defun send-command (cmd &rest args)
  "Send CMD and ARGS to Moxie's generic result handler."
  (let ((*print-pretty* nil))
    (prin1 `(,cmd ,@args) *moxie-result-stream*))
  #-clisp (finish-output *moxie-result-stream*))
