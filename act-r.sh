#!/bin/bash
set -e
set -m

#sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --eval "(print (run-the-model-until-time 10 (list \"arm\" \"adaptive\") 39 \"high\" nil 20 5))"

sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --load app.lisp
#sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --eval "(ql:quickload '(clack caveman2))"
#sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --eval "(ql:quickload :web)" --eval "(web:start :port 8080)"
#sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --eval "(ql:quickload :web)"
