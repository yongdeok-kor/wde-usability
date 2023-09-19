;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Hyungseok Oh
;;; Address     : Cognitive Ergonomic Lab.
;;;             : Department of Industrial Engineering 
;;;             : Korea University
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : arm-module.lisp
;;; Version     : 0.1
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Module to support parameters and device internals.

(defstruct arm-module 
  busy 
  mode
  angle
  resistance
  res-thres
  ang-vel
  desired-ang-vel
  peak-angle
  (lock (bt:make-recursive-lock "arm"))
  )

(defun create-arm-module (x)
  (declare (ignore x))
  (make-arm-module))

(defun reset-arm-module (x)
  (declare (ignore x))
  ;(chunk-type demo-output value (demo-output t))
  )

(defun delete-arm-module (x)
  (declare (ignore x)))

(defun arm-module-params (arm param)
  (if (consp param)
    (case (car param)
      (:set-peak-angle
       (setf (arm-module-peak-angle arm) (cdr param)))
      (:set-resistance-threshold
       (setf (arm-module-res-thres arm) (cdr param)))
      (:set-arm-mode
       (setf (arm-module-mode arm) (cdr param)))
      )
    (case param
      (:set-peak-angle
       (arm-module-peak-angle arm))
      (:set-resistance-threshold
       (arm-module-res-thres arm))
      (:set-arm-mode
       (arm-module-mode arm)))))


(defun arm-module-queries (arm buffer query value)
  (case query
    (state
     (case value
       (busy (arm-module-busy arm))
       (free (not (arm-module-busy arm)))
       (error nil)
       (t (print-warning "Bad state query to the ~s buffer" buffer))))
    (t (print-warning "Invalid query ~s to the ~s buffer" query buffer))))




(define-module-fct 'arm
                   '(angle resistance) 
  (list (define-parameter :set-peak-angle 
               :documentation 
                  "set peak angle of arm movements"
               :default-value 90
               :valid-test 'nonneg
               :warning "Non-negative number"
               :owner t)
	(define-parameter :set-resistance-threshold 
               :documentation 
                  "set threshold of arm movements"
               :default-value .5
               :valid-test 'nonneg
               :warning "Non-negative number"
               :owner t)
	(define-parameter :set-arm-mode 
               :documentation 
                  "set mode of arm movements (t: fixed)"
               :default-value t
               :valid-test 'tornil
               :warning "T or nil"
               :owner t)
        )

   ;:request 'arm-module-requests
   :query 'arm-module-queries

   :version "1.0a1"
   :documentation "arm module for wearable device"
   :creation 'create-arm-module
   :reset 'reset-arm-module 
   :delete 'delete-arm-module
   :params 'arm-module-params
   )



;;;
;;; update motor module
;;;

(defstyle arm-movement () peak ang-vel)

(defmethod feat-differences ((f1 arm-movement) (f2 arm-movement))
  (let ((count 0))
    (unless (equal (peak f1) (peak f2))
      (incf count))
    (unless (equal (ang-vel f1) (ang-vel f2))
      (incf count))
    count))

(defmethod compute-exec-time ((m motor-module) (self arm-movement))
  (let ((arm (get-module arm)))
    (unless (arm-module-ang-vel arm) (setf (arm-module-ang-vel arm) 7))
    (unless (arm-module-desired-ang-vel arm) (setf (arm-module-desired-ang-vel arm) 7))
    (if (arm-module-mode arm)
	(progn
	  (setf (arm-module-ang-vel arm) 7)
	  (setf (arm-module-desired-ang-vel arm) (+ (arm-module-ang-vel arm) (act-r-noise .3)))
	  )
      (progn
	(when (> (abs (- (arm-module-ang-vel arm) (arm-module-desired-ang-vel arm))) (arm-module-res-thres arm))
	  (setf (arm-module-resistance arm) (abs (- (arm-module-ang-vel arm) (arm-module-desired-ang-vel arm))))
	  (setf (arm-module-ang-vel arm) (arm-module-desired-ang-vel arm))
	  (setf (arm-module-desired-ang-vel arm) (+ (arm-module-ang-vel arm) (act-r-noise .3)))
	  ))
      )
    
  )
  .4)

(defmethod compute-finish-time ((m motor-module) (self arm-movement))
  ;; .1 second longer than the execution time
  (+ .1 (exec-time self)))

(defmethod queue-output-events ((m motor-module) (self arm-movement))
  
  ;; Here we are just scheduling an event to print out the details
  ;; of the request at the execution time.
  
  (schedule-event-relative (exec-time self) 
                           'arm-print-style-info
                           :params (list (peak self) (ang-vel self) (fprep-time self) (finish-time self)))
  
  ;; If the action were moving the hand then it should schedule that
  ;; here using the set-hand-position action with the value that was
  ;; stored in updated-pos by the prepare-features method.  Again, this
  ;; example does not need to do so, but this example is from the peck 
  ;; style movement:
  
  #|
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                           :output nil :destination :motor 
                           :params (list (hand self) (updated-pos self)))
  |#
  )


;;; The function that will be called:

(defun arm-print-style-info (w x y z) 
  ;(format t "Features are ~S and ~S~%~s seconds spent in preparation~%finishing after ~f seconds~%" w x y z)
  )

;;; To add the new style use extend-manual-requests and provide the 
;;; style name and features as the description of the action and
;;; use the built-in function handle-style-request to process the
;;; requests.

(extend-manual-requests (arm-movement peak ang-vel) handle-style-request)


;;;
;;; ARM POSITION FUNCTIONS!!
;;;

;;; Trajectory function
(defparameter *peak-angle* 90)
(defparameter *fixed-ang-vel* 7)

(defun arm-trajectory-func (time peak ang-vel)
  (let ((angle 0)
	(term (* (/ ang-vel peak) time)))
    (setf angle (* peak (+ (- (* 10 (expt term 3)) (* 15 (expt term 4))) (* 6 (expt term 5)))))
    angle
  ))

(defun get-angle-arm-movement (time)
  (let* ((arm (get-module arm))
	(peak (arm-module-peak-angle arm))
	(ang-vel (arm-module-ang-vel arm)))
    (arm-trajectory-func time peak ang-vel)))

(add-act-r-command "get-angle-arm-movement" 'get-angle-arm-movement)


(defun get-resistance-for-arm ()
  (let ((arm (get-module arm)))
    (if (> (abs (- (arm-module-ang-vel arm) (arm-module-desired-ang-vel arm))) (arm-module-res-thres arm))
	(abs (- (arm-module-ang-vel arm) (arm-module-desired-ang-vel arm)))
      0)
  
  ))

(add-act-r-command "get-resistance-for-arm" 'get-resistance-for-arm)