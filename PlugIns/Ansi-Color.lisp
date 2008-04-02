(defpackage ansi-color
  (:use :cl :cl-user :moxie :bjc-utils))
(in-package :ansi-color)

(defvar *black-color* '(0 0 0))
(defvar *red-color* '(200 0 0))
(defvar *green-color* '(0 200 0))
(defvar *yellow-color* '(200 200 0))
(defvar *blue-color* '(0 0 200))
(defvar *purple-color* '(200 0 200))
(defvar *cyan-color* '(0 200 200))
(defvar *white-color* '(200 200 200))

(defun active-attributes (&optional (world *world*))
  (world-var 'active-attrs world))

(defsetf active-attributes (&optional (world '*world*)) (values)
  `(setf (world-var 'active-attrs ,world) ,values))

(defun add-attribute (attribute)
  (remove-attribute (car attribute))
  (setf (active-attributes)
        (cons attribute (active-attributes))))

(defun attribute-value (attribute)
  (cdr (assoc attribute (active-attributes))))

(defun remove-attribute (attribute)
  (setf (active-attributes)
        (remove attribute (active-attributes) :key #'car)))

(defun set-attribute-for-code (code)
  (case code
    (0 (when (active-attributes)
         (setf (active-attributes) nil)))
    (1 (add-attribute '(:bold 1)))
    (3 (add-attribute '(:italic 0.25)))
    (4 (add-attribute '(:underline 1)))
    (7 (add-attribute '(:inverse 1)))
    (9 (add-attribute '(:strikethrough 1)))
    (22 (remove-attribute :bold))
    (23 (remove-attribute :italic))
    (24 (remove-attribute :underline))
    (27 (remove-attribute :inverse))
    (29 (remove-attribute :strikethrough))
    (30 (add-attribute (cons :color *black-color*)))
    (31 (add-attribute (cons :color *red-color*)))
    (32 (add-attribute (cons :color *green-color*)))
    (33 (add-attribute (cons :color *yellow-color*)))
    (34 (add-attribute (cons :color *blue-color*)))
    (35 (add-attribute (cons :color *purple-color*)))
    (36 (add-attribute (cons :color *cyan-color*)))
    (37 (add-attribute (cons :color *white-color*)))
    (39 (remove-attribute :color))
    (40 (add-attribute (cons :background-color *black-color*)))
    (41 (add-attribute (cons :background-color *red-color*)))
    (42 (add-attribute (cons :background-color *green-color*)))
    (43 (add-attribute (cons :background-color *yellow-color*)))
    (44 (add-attribute (cons :background-color *blue-color*)))
    (45 (add-attribute (cons :background-color *purple-color*)))
    (46 (add-attribute (cons :background-color *cyan-color*)))
    (47 (add-attribute (cons :background-color *white-color*)))
    (49 (remove-attribute :background-color))))

(defun end-of-ansi-position (string start)
  "Returns the position in STRING after START where an ANSI escape sequence ends."
  (position-if (lambda (c)
                 (or (and (char< c #\z) (char> c #\a))
                     (and (char< c #\Z) (char> c #\A))))
               string :start start))

(defun colorize-ansi-string (string)
  (declare (ignore keywords))
  (let* (*print-pretty*
         (string (concatenate 'string
                              (aif (world-var 'cached-escape)
                                   (progn
                                     (setf (world-var 'cached-escape) nil)
                                     it)
                                    "")
                              (if (stringp string) string (car string))))
         (scanlen (length string))
         (attr-index 0)
         (attributes nil)
         (skipped-chars 0)
         (result (with-output-to-string (stripped-string)
                   (let ((final-scan (do* ((i 0 (1+ i)))
                                          ((>= i scanlen) i)
                                       (when (eql (elt string i) #\Escape)
                                         (aif (end-of-ansi-position string i)
                                              (progn
                                                ;; When we have a code, we should dump the last
                                                ;; set of attributes.
                                                (princ (subseq string attr-index i) stripped-string)
                                                (awhen (active-attributes)
                                                  (push (cons (make-range (- attr-index skipped-chars)
                                                                          (- i attr-index))
                                                              (active-attributes)) attributes))
                                           
                                                ;; Grab the code sequence, break it up, and set the
                                                ;; attributes for it.
                                                (let ((code-str (subseq string (+ i 2) it)))
                                                  (flet ((code-in-bounds (start end)
                                                           (let ((start (or start 0))
                                                                 (end (or end (length code-str))))
                                                             (set-attribute-for-code (or (and (= start end) 0)
                                                                                         (parse-integer (subseq code-str start end)
                                                                                                        :junk-allowed t))))))
                                                    ;; Coalesce attributes split up by semicolons.
                                                    (do* ((last-pos 0 (1+ semi-pos))
                                                          (semi-pos (position #\; code-str)
                                                                    (position #\; code-str :start last-pos))
                                                          (code (code-in-bounds last-pos semi-pos)
                                                                (code-in-bounds last-pos semi-pos)))
                                                         ((null semi-pos)))))
                                                (setf skipped-chars (+ skipped-chars (- (1+ it) i)))
                                                (setf i it)
                                                (setf attr-index (1+ i)))
                                              ;; We have an escape, but can't parse it.
                                              ;; save it in a buffer for later use.
                                              (progn
                                                (setf (world-var 'cached-escape) (subseq string i scanlen))
                                                (setf scanlen i)))))))
                     ;; Append final attributes and string.
                     (when (<= attr-index scanlen)
                       (princ (subseq string attr-index scanlen) stripped-string)
                       (push (cons (make-range (- attr-index skipped-chars) (- scanlen attr-index))
                                   (active-attributes)) attributes))))))
    (if attributes
        (apply #'make-attributed-string result attributes)
        result)))

(defun test-output (&rest vars)
  (with-output-to-string (s)
    (dolist (v vars)
      (format t "first char: ~S, var ~S~%" (elt v 0) v)
      (princ v s))))

(defun printer-test ()
  (test-output "string-1" (format nil "string-2~%")))

(defun make-ansi-sequence ()
  (format nil "~C[~A;~Am~A ~C[~A;~A;~AmAnd another" #\Escape 0 31 "A sequence" #\Escape 1 22 37))

(add-hook 'colorize-ansi-string :output-from-server-hook)
