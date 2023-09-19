#!/bin/sh
sudo apt-get update; sudo apt-get install unzip 

wget http://prdownloads.sourceforge.net/sbcl/sbcl-2.0.9-x86-64-linux-binary.tar.bz2
tar -xf sbcl-2.0.9-x86-64-linux-binary.tar.bz2 && rm sbcl-2.0.9-x86-64-linux-binary.tar.bz2
cd sbcl-2.0.9-x86-64-linux && sudo sh install.sh && cd .. && rm -r sbcl-2.0.9-x86-64-linux
wget https://beta.quicklisp.org/quicklisp.lisp && sbcl --quit --load quicklisp.lisp --eval '(quicklisp-quickstart:install :path "quicklisp")' && rm quicklisp.lisp
wget http://act-r.psy.cmu.edu/actr7.x/actr7.x.zip && unzip actr7.x.zip && rm -r actr7.x.zip

/bin/sh -c sbcl --quit --load quicklisp/setup.lisp --eval '(push :standalone *features*)' --load actr7.x/load-act-r.lisp

# install clack
#/bin/sh -c sbcl --quit --load quicklisp/setup.lisp --eval "(ql:quickload :clack)"
/bin/sh -c sbcl --quit --load quicklisp/setup.lisp --eval "(ql:quickload '(:ningle :djula :dexador :cl-json))"
