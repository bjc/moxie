(load "../asdf/asdf")
(pushnew (merge-pathnames ".lisp/systems/" (user-homedir-pathname))
         asdf:*central-registry*)
(asdf:operate 'asdf:load-op :moxie)
(moxie::save-lisp-and-die "/tmp/lispinit.mem")
