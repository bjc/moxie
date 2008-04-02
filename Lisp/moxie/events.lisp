(in-package :moxie)

(defgeneric moxie-event-handler (event &rest args)
  (:documentation "Handle EVENT (w/ ARGS)."))

(defmethod moxie-event-handler ((event (eql :world-event)) &rest args)
  (apply #'world-event args))

(defmethod moxie-event-handler ((event (eql :eval)) &rest args)
  (do* ((f args (cdr f))
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

(defmethod world-event-handler ((event (eql :close-world)) &rest args)
  (declare (ignore args))
  (close-world))

(defmethod world-event-handler ((event (eql :connect-world)) &rest args)
  (declare (ignore args))
  (world-connect))

(defmethod world-event-handler ((event (eql :disconnect-world)) &rest args)
  (declare (ignore args))
  (world-disconnect))

(defmethod world-event-handler ((event (eql :load-world)) &rest args)
  (format t "world-event-handler :load-world ~S~%" args)
  (apply #'load-world-state *world* args))

(defmethod world-event-handler ((event (eql :save-world)) &rest args)
  (apply #'save-world-state *world* args))

(defmethod world-event-handler ((event (eql :setting-changed)) &rest args)
  (let* ((form (car args))
         (key (car form))
         (val (cadr form))
         (old-val (world-var key)))
    (unless (eql old-val val)
      (format t "DEBUG: changing setting ~S: ~S -> ~S.~%" key old-val val)
      (setf (world-var key) val)
      (format t "DEBUG: running hook.~%")
      (run-hook :setting-changed-hook (list key val old-val))
      (format t "DEBUG: hook finished.~%"))))

(defmethod world-event-handler ((event (eql :input-from-client-hook)) &rest args)
  (send-to-mux *world* (or (run-hook event (car args)) (car args))))

(defmethod load-world-state ((world world) &key path &allow-other-keys)
  (format t "load-world-state ~S ~S~%" world path)
  (with-open-file (s (or path (world-save-path world)))
    (awhen (aand (read s) (parse-world-version-1 it))
      (setf (world-vars world) it)
      (setf (world-save-path world) path)
      (let ((*world* world))
        (run-hook :world-loaded-hook)))))

(defmethod save-world-state ((world world) &key path as-copy &allow-other-keys)
  (with-open-file (s (or path (world-save-path world))
                     :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
    (prin1 (write-world-version-1) s))
  (unless as-copy
    (setf (world-save-path world) path))
  (let ((*world* world))
    (run-hook :world-saved-hook)))

(defun parse-world-version-1 (form)
  "Parses a world definition in the form '(:KEY value), returning an ALIST."
  (when (evenp (length form))
    (labels ((keyvalue-to-alist (form &optional (accumulator nil))
               (if (null form)
                   accumulator
                   (keyvalue-to-alist (cddr form)
                                      (cons (cons (car form) (cadr form)) accumulator)))))
      (keyvalue-to-alist form))))

(defun write-world-version-1 (&optional (world *world*))
  "Writes out a FORM of '(:KEY1 value1 :KEY2 value2) from WORLD."
  (labels ((alist-to-keyvalue (form &optional (accumulator nil))
             (if (null form)
                 accumulator
                 (alist-to-keyvalue (cdr form)
                                    (cons (caar form) (cons (cdar form) accumulator))))))
    (alist-to-keyvalue (world-vars world))))