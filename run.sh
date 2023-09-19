#!/bin/bash
set -e
set -m

# ningle
sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --load app.lisp

# clack
#sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --load clack-server.lisp
