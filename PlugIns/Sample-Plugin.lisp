#-cl-ppcre (asdf:operate 'asdf:load-op :cl-ppcre)
(defpackage sample-plugin
  (:use :cl :cl-user :moxie :bjc-utils :cl-ppcre))
(in-package :sample-plugin)

(defvar *page-highlight-attrs* (list (make-color 240 10 240)))

(defun print-mandel (&optional (stream *standard-output*))
  "Prints a mandelbrot set to STREAM."
  (loop for y from -1 to 1.1 by 0.1 do
       (format stream "~%")
       (loop for x from -2 to 1 by 0.04 do
            (let* ((c 126)
                   (z (complex x y))
                   (a z))
              (loop while (< (abs
                              (setq z (+ (* z z) a)))
                             2)
                 while (> (decf c) 32))
              (princ (code-char c) stream)))))

(defun mandel-in-string ()
  (escape-mux-string (with-output-to-string (s)
                       (print-mandel s)
                       s)))

(defun mandel-page (string)
  "Sends a mandelbrot set to the first arg in STRING."
  (map-variables "p $1$=$2$"
                 (list (car (split "\\s+" string))
                       (mandel-in-string))))

(defun highlight-pages (string)
  "Highlights a page if it comes in."
  (let ((string (if (stringp string) string (car string))))
    (multiple-value-bind (match names)
        (scan-to-strings "^((.*)\\s+pages:|From afar, (\\w+))" string)
      (when match
        (format t "You were paged by: ~A.~%" (elt names 1))
        (make-attributed-string string *page-highlight-attrs*)))))

(defun complex-attribute (keystroke)
  (let ((attr-string (make-attributed-string "Foobarbaz"
                                             `(,(make-color 255 255 255) (:italic 0.25))
                                             `(,(make-range 3 3)
                                                ,(make-super 1)
                                                ,(make-color 127 127 127))
                                             `(,(make-range 6 3)
                                                ,(make-super 2)
                                                ,(make-color 63 63 63)))))
    (format t "complex attribute: ~S~%" attr-string)
    (print-to-world *world* attr-string)))

;; Now that we have the functions defined, hook 'em into Moxie.
(add-hook 'highlight-pages :output-from-server-hook)

; Register MANDEL-PAGE for the command "/MANDEl"
(add-keyword 'mandel-page "mandel")

(add-keystroke-macro 'complex-attribute :f1)
