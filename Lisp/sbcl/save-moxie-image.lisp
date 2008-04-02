(require 'asdf)
(asdf:operate 'asdf:load-op :moxie)
(moxie::save-lisp-and-die "/tmp/sbcl.core")
