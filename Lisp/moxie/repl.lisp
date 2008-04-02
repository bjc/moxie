(in-package :moxie)

(defvar *repl-motd*
  "Welcome to Moxie!

To get help, enter :HELP at the prompt.")

(defvar *repl-help*
  "Top level commands:
    :R [num]     Invoke restart NUM, or list restarts.
    :HELP :H :?  Display this message.")

(defvar *repl-level* 0)

(defun start-repl (&optional (use-result-stream t))
  (let ((*moxie-result-stream* (or (and use-result-stream (make-result-stream))
                                   *error-output*)))
    (format t "~%~A~%" *repl-motd*)
    (send-command :repl-result `(:prompt ,(repl-prompt)))
    (repl)))

(defun repl ()
  "This is Moxie's top level loop. At this point, it's only here
because we don't want the host lisp to print results or its prompt."
  (let* ((*debugger-hook* #'repl-dbg)
         (*repl-level* (1+ *repl-level*))
         (lex-level *repl-level*))
    (loop
       (force-output)
       (let ((form (read)))
         (restart-case (eval form)
           (abort ()
             :report (lambda (stream)
                       ;; I know this looks weird, but because the
                       ;; formatter is called from the condition
                       ;; handler's environment, and because
                       ;; *repl-level* is special, at the time of
                       ;; evaluation, *repl-level* may be higher than
                       ;; lex-level.
                       (if (eql lex-level *repl-level*)
                           (format stream "Abort handling of current request.")
                           (format stream "Return to REPL level ~A."
                                   lex-level)))
             (send-command :repl-result `(:prompt ,(repl-prompt)))))))))

(defun repl-dbg (condition debugger-hook)
  "This debugger hook just sends a message to Moxie when the debugger
has been entered, so Moxie can keep track of the prompt."
  (declare (ignore debugger-hook))
  (send-command :repl-dbg `(:condition ,condition)))

(defmacro eval-hook (&rest forms)
  "Ensure all FORMS are valid for evaluation before calling
EVAL-HOOK-HELPER."
  (let ((helped-forms (mapcar (lambda (x) `(quote ,x)) forms)))
    `(eval-hook-helper ,@helped-forms)))

(defun eval-hook-helper (&rest forms)
  "Evaluate all FORMS, sending the results to the Moxie output
stream. When finished processing, send the prompt."
  (do* ((f forms (cdr f))
        (form (car f) (car f)))
       ((null f))
    (case form
      (:r (let ((restarts (compute-restarts))
                (num (cadr f)))
            (if (and (integerp num)
                     (> num 0) (<= num (length restarts)))
                (progn
                  (setf f (cdr f))
                  (invoke-restart (elt restarts (1- num))))
                (print-restarts restarts))))
      ((:? :h :help) (format t "~A~%" *repl-help*))
      (t (let (values)
           (setq - form)
           (setq values (multiple-value-list (eval -)))
           (setq /// // // / / values *** ** ** * * (car /))
           (send-command :repl-result `(:values ,@values))))))
  (send-command :repl-result `(:prompt ,(repl-prompt))))

(defun print-restarts (restarts)
  (format t "Available restarts: ~%")
  (do ((c restarts (cdr c))
       (i 1 (1+ i)))
      ((null c))
    (format t " ~A ~A~%" i (car c)))
  (format t "Invoke restarts with :R [num]~%"))

(defun repl-prompt ()
  "Compute the prompt for Moxie's REPL."
  (format nil "~A~@[[~A]~]> "
          (if (eql *package* (find-package :cl-user))
              "CL-USER"
              (package-name *package*))
          (when (> *repl-level* 1) *repl-level*)))