;;; -*- Lisp -*-
;; $Id: bjc-utils.lisp 19 2005-12-27 01:40:27Z bjc $
(in-package :moxie)

(defmacro while (expr &body body)
  "Evaluate BODY continously until EXPR evaluates to FALSE."
  `(do ()
       ((not ,expr))
     ,@body))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cl1)))
	  (if ,sym
	      (let ((it ,sym))
		,@(cdr cl1)
		(acond ,@(cdr clauses))))))))

(defmacro aif (expr then &optional else)
  "Anaphoric if: if EXPR is true, set IT to the result of EXPR and evaluate THEN, otherwise evaluate ELSE."
  `(let ((it ,expr))
     (if it
	 ,then
       ,else)))

(defmacro awhen (expr &body body)
  "Anaphoric when: when EXPR is true, set IT to the result of EXPR and evaluate BODY."
  `(let ((it ,expr))
    (when it
      ,@body)))

(defmacro awhile (expr &body body)
  "Anaphoric while: while EXPR is true, set IT to the result of EXPR and evaluate BODY."
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro aif2 (expr &optional then else)
  "Two-value version of aif: aif EXPR's second value is TRUE, evaluate THEN, otherwise, evaluate ELSE."
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,expr
       (if (or it ,win) ,then ,else))))

(defmacro awhile2 (expr &body body)
  "Two-value version of awhile: awhile EXPR's second value is TRUE, evaluate BODY."
  (let ((flag (gensym)))
    `(let ((,flag t))
      (while ,flag
	(aif2 ,expr
	      (progn ,@body)
	  (setq ,flag nil))))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s) `(,s (gensym))) syms)
     ,@body))

(declaim (ftype (function (function) function) memoize))
(defun memoize (f)
  "Return memoized version of FN."
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win
            val
            (setf (gethash args cache) (apply f args)))))))

(declaim (ftype (function (function integer) function) memoize-with-timeout))
(defun memoize-with-timeout (fn len)
  "Memoize FN for LEN seconds after initial call."
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if (and win (< (get-universal-time) (car val)))
            (cdr val)
            (cdr (setf (gethash args cache)
                       (cons (+ len (get-universal-time))
                             (apply fn args)))))))))

(defmacro enumerator (list)
  "Returns an enumerator for LIST."
  (let ((index (gensym)))
    `(let ((,index 0))
      (lambda ()
        (progn
          (incf ,index)
          (nth (1- ,index) ,list))))))

(defun mkstr (&rest args)
  "Creates a str from ARGS."
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

;;
;; This macro can save and load the state of simple variables.
;;
;; Use:
;; > (setq *foo* '(1 2 3)) => (1 2 3)
;; > (def-i/o foo-w foo-r (*foo*)) => T
;; > (foo-w #p"/tmp/foo-vars") => NIL
;; > (makunbound '*foo*) => *FOO*
;; > (foo-r #p"/tmp/foo-vars") => NIL
;; > *foo* => (1 2 3)
(defmacro def-i/o (writer-name reader-name (&rest vars))
  (let ((file-name (gensym))
        (var (gensym))
        (stream (gensym)))
    `(progn
       (defun ,writer-name (,file-name)
         (with-open-file (,stream ,file-name
                                  :direction :output :if-exists :supersede)
           (dolist (,var (list ,@vars))
             (declare (special ,@vars))
             (print ,var ,stream))))
       (defun ,reader-name (,file-name)
         (with-open-file (,stream ,file-name
                                  :direction :input :if-does-not-exist :error)
           (dolist (,var ',vars)
             (set ,var (read ,stream)))))
       t)))

(defun string-has-prefix (string prefix)
  "Returns T if STRING begins with PREFIX, NIL otherwise."
  (let ((strlen (length string))
        (prefixlen (length prefix)))
    (when (<= prefixlen strlen)
      (do ((i 0 (1+ i)))
          ((<= prefixlen i) t)
        (let ((s (elt string i)) (p (elt prefix i)))
          (when (not (eql s p))
            (return-from string-has-prefix nil)))))))

(defmacro llambda (simple-lambda-list &body body)
  (let ((num-args (gensym))
        (args (gensym))
        (accumulated-args (gensym))
        (call-lambda (gensym)))
    (labels ((lambda-length (simple-lambda-list &optional (count 0))
               (if (or (null simple-lambda-list)
                       (member (car simple-lambda-list)
                               '(&allow-other-keys &key &rest &aux &optional)))
                   count
                   (lambda-length (cdr simple-lambda-list) (1+ count)))))
      `(labels ((,call-lambda (,num-args ,accumulated-args)
                  (lambda (&rest ,args)
                    (if (< (length ,args) ,num-args)
                        (,call-lambda (- ,num-args (length ,args))
                                      (append ,accumulated-args ,args))
                        (apply (lambda ,simple-lambda-list ,@body)
                               (append ,accumulated-args ,args))))))
         (,call-lambda ,(lambda-length simple-lambda-list) nil)))))

(defmacro $c (f &rest args)
  (let ((a (gensym)))
    `(lambda ($_)
       (flet ((my-apply (sym args)
                (cond ((functionp sym) (apply (the function sym) args))
                      ((macro-function sym)
                       (eval (funcall (macro-function sym)
                                      `(,sym ,args)
                                      nil)))
                      ((symbol-function sym) (apply (symbol-function sym) args))
                      (t (error "Can't curry ~A" (type-of sym))))))
         (let ((,a (subs-var '$_ $_
                             (list ,@(if (member '$_ args)
                                         args
                                         (append args '($_)))))))
           (my-apply ,f ,a))))))

(defun subs-var (sym val expr &optional accum)
  (if (null expr)
      (nreverse accum)
      (subs-var sym val (cdr expr)
                (if (and (atom (car expr))
                         (eq (car expr) sym))
                    (cons val accum)
                    (cons (car expr) accum)))))