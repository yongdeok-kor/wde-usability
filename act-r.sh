#!/bin/bash
set -e
set -m

SECONDS=$1

#sbcl --load "quicklisp/setup.lisp" --load "actr-usability/load-act-r.lisp" --load "actr-usability/usability/1.0/model_parameters.lisp" --load "actr-usability/usability/1.0/model/simple_model.lisp" --load "actr-usability/usability/1.0/task/simple_arm_movement_task.lisp" --load "actr-usability/usability/system_interface.lisp" --eval "(run-the-model-until-time ${SECONDS} (list \"arm\" \"adaptive\") 39 \"high\" nil 20 5)"
sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" 
#--load "actr7.x/usability/system_interface.lisp" --eval "(run-the-model-until-time 10 (list \"arm\" \"adaptive\") 39 \"high\" nil 20 5)"
#sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --eval "(run-the-model-until-time ${SECONDS} (list \"arm\" \"adaptive\") 39 \"high\" nil 20 5)"
#sbcl --load "quicklisp/setup.lisp" --load "actr7.x/load-act-r.lisp" --load "actr7.x/usability/system_interface.lisp" --eval "(run-the-model-until-time ${SECONDS} (list \"arm\" \"adaptive\") 39 \"high\" nil 20 5)"

#(load "/home/donghee/src/wearable_robot_act-r/tmp/actr7.x/usability/system_interface.lisp")
#(run-the-model-until-time 10 (list "arm" "adaptive") 39 "high" nil 20 5)