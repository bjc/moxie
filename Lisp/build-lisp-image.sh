#!/bin/sh

PATH="$HOME/bin:$PATH"
export PATH

build_clisp()
{
    clisp -i save-moxie-image.lisp
    gzip -c /tmp/lispinit.mem > base/lispinit.mem
    rm -f /tmp/lispinit.mem
}

build_openmcl()
{
    openmcl -e '(load "save-moxie-image.lisp")'
    mv /tmp/dppccl.image .
}

build_sbcl()
{
    sbcl --load "save-moxie-image.lisp"
    mv /tmp/sbcl.core .
}

topdir=`dirname $0`

mkdir $topdir/clisp/base
#(cd "$topdir/clisp" && build_clisp)
#(cd "$topdir/openmcl" && build_openmcl)
(cd "$topdir/sbcl" && build_sbcl)
