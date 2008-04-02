;;; -*- Lisp -*-
;; $Id: world.lisp 48 2006-01-09 00:27:16Z bjc $
(in-package :moxie)

(defvar *worlds* (make-hash-table)
  "The world environments, keyed on world id.")

(let ((next-world-id 0))
  (defclass world ()
    ((id :initarg :id :initform (incf next-world-id)
         :accessor world-id
         :documentation "The world id.")
     (vars :initarg :vars :initform nil
           :accessor world-vars
           :documentation "Savable settings.")
     (save-path :initarg :save-path :initform nil
                :accessor world-save-path
                :documentation "File path.")
     (stream :initarg :stream :initform nil
             :accessor world-stream
             :documentation "Connection to server.")
     (connected :initarg :connected :initform nil
                :accessor world-connected
                :documentation "Are we currently connected?"))
    (:documentation "All associated world information.")))

(defgeneric load-world-state (world &key path &allow-other-keys)
  (:documentation "Returns an ALIST from WORLD's disk location, or PATH (if set)."))

(defgeneric save-world-state (world &key path as-copy &allow-other-keys)
  (:documentation "Saves WORLD's state to its disk location or PATH (if set)."))

(defgeneric world-event-handler (event &rest args)
  (:documentation "Handle EVENT (w/ ARGS) for *WORLD*."))

(defmethod initialize-instance ((instance world) &rest initargs)
  (declare (ignore initargs))
  (format t "initialize-instance world~%")
  (add-world (call-next-method)))

(defmethod world-event-handler (event &rest args)
  "Default handler doesn't know about anything, so it logs, instead."
  (format t "Don't know how to handle event ~S ~S from world ~S.~%"
          event args (world-id *world*)))

(defun add-world (world)
  (setf (gethash (world-id world) *worlds*) world))

(defun remove-world (world)
  (remhash (world-id world) *worlds*))

(defun map-worlds (fn)
  (let ((result nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (setf result (cons (funcall fn v) result)))
             *worlds*)
    (nreverse result)))

(defun map-world-vars (fn &optional (world *world*))
  (mapcar (lambda (list)
            (funcall fn (car list) (cdr list)))
          (world-vars world)))

(defun world-var (name &optional (world *world*))
  "Returns the value for NAME in WORLD's environment."
  (cdr (assoc name (world-vars world))))

(defun set-world-var (name value &optional (world *world*))
  "Sets the value of NAME to VALUE in WORLD's environment."
  (setf (world-vars world)
        (cons (cons name value)
              (remove-if (lambda (x)
                           (eql (car x) name))
                         (world-vars world)))))

(defsetf world-var (name &optional (world '*world*)) (value)
  `(set-world-var ,name ,value ,world))

(defun close-world (&optional (world *world*))
  "Closes WORLD."
  (world-disconnect world)
  (remove-world world)
  (let ((*world* world))
    (run-hook :world-closed-hook)))

(defun world-connect (&optional (world *world*))
  "Connects WORLD to the host and port specified."
  (awhen (aand (world-var :hostname world) (world-var :port world)
               (open-connection (world-var :hostname world) (world-var :port world)))
    (add-input-handler it
                       (lambda (stream)
                         (let ((*world* world))
                           (handler-case
                               (while (listen stream)
                                 (multiple-value-bind (line missing-newline-p) (read-line stream)
                                   (run-hook :output-from-server-hook line)
                                   (when missing-newline-p
                                     (signal 'end-of-file))))
                             (end-of-file ()
                               (world-disconnect world))))))
    (setf (world-stream world) it)
    (setf (world-connected world) t)
    (let ((*world* world))
      (run-hook :world-connected-hook))))

(defun world-disconnect (&optional (world *world*))
  "Closes the connection, if opened, for WORLD."
  (let ((*world* world))
    (when (world-connected *world*)
      (close-connection (world-stream *world*))
      (setf (world-stream *world*) nil)
      (setf (world-connected *world*) nil)
      (run-hook :world-disconnected-hook))))

(defun world-event (world-id &rest args)
  (format t "DEBUG: world-event ~S ~S~%" world-id args)
  (let ((*world* (or (gethash world-id *worlds*)
                     (make-instance 'world :id world-id))))
    (apply #'world-event-handler args)))