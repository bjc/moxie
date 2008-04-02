;;; -*- Lisp -*-
;; $Id: moxie.asd 33 2006-01-01 06:41:36Z bjc $
(defpackage moxie-system
  (:use :cl :asdf))
(in-package :moxie-system)

(defsystem :moxie
    :name "Moxie REPL Components."
    :version "0.2"
    :author "Brian Cully <shmit@kublai.com>"
    :maintainer "Brian Cully <shmit@kublai.com>"
    :licence "Public Domain"
    :description "Moxie's Lisp programming interface."

    :depends-on (#+sbcl sb-bsd-sockets)
    :components ((:file "package")
                 (:module "utils"
                          :components ((:file "bjc-utils"))
                          :depends-on ("package"))
                 (:module "compat"
                          :components ((:file #+sbcl "compat-sbcl"
                                              #+clisp "compat-clisp"
                                              #+openmcl "compat-openmcl"
                                              #-(or sbcl clisp openmcl) (error "Compiler not supported.")))
                          :depends-on ("package"))
                 (:module "main"
                          :pathname ""
                          :components ((:file "moxie")
                                       (:file "world" :depends-on ("moxie"))
                                       (:file "events" :depends-on ("world"))
                                       (:file "repl" :depends-on ("world"))
                                       (:file "default" :depends-on ("moxie")))
                          :depends-on ("package" "compat" "utils"))))
(pushnew :moxie *features*)
