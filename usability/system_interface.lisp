
;;;;;;;;;;;;
;; 
;; SYSTEM INTERFACE MODULE - first version
;;
;; Hyungseok Oh
;;
;;;;;;;;;;;;
;;
;; This module processes the system interface including input/output parameters in the digital human twin
;;
;;;;;;;;;;;;
;;
;; 2013.07.16
;; ** Initial creation of the module
;;
;;;;;;;;;;;;

;(actr-load "ACT-R:usability;1.0;model_parameters.lisp")
;(actr-load "ACT-R:usability;1.0;model;simple_model.lisp")
;(actr-load "ACT-R:usability;1.0;task;simple_arm_movement_task.lisp")

(load "/home/donghee/src/wearable_robot_act-r/tmp/actr7.x/usability/1.0/model_parameters.lisp")
(load "/home/donghee/src/wearable_robot_act-r/tmp/actr7.x/usability/1.0/model/simple_model.lisp")
(load "/home/donghee/src/wearable_robot_act-r/tmp/actr7.x/usability/1.0/task/simple_arm_movement_task.lisp")

;;;
;;; Run the simulation model
;;;
(defun run-the-model-until-time (time task age cog-level trajectory force noise)  
                              ;; time -- seconds 
                              ;; task - list 2 params -- "arm" // "adaptive" or "fixed"
                              ;; age  -- twin age
                              ;; cog-level -- "high" "low"
                              ;; trajectory -- list 2 params -- peak, angular-velocity
                              ;; force -- percentage value
                              ;; noise -- individual difference value - under 10
  
  (let ((total-usability-value 0)
	(learnability     0)
	(memorability     0) ;do not use yet
	(utility          0) 
	(efficiency       0) ;mental workload
	(effectiveness    0) ;human error
	(performance-time 0)
	(error-value      0)
	(model-output-text))

  (reset)
  
  ;;;setting parameters
  (input-task-information-parameter task)
  (input-twin-cognitive-ability-parameter cog-level)
  (input-device-force-timeline-parameter force)
  (input-twin-noise-parameter noise)
  
  (setting-twin-parameters-for-model *SETTING-INPUT-PARAMETERS*)
   
  ;;;run the model
  (setf model-output-text (do-test-until-time time))
  
  ;;;calculate the simulation output parameters
  (setf learnability  (get-value-learnability-parameter))
  (setf memorability  (get-value-memorability-parameter))
  (setf utility       (get-value-utility-parameter))
  (setf efficiency    (get-value-mental-workload-parameter))
  (setf effectiveness (get-value-effectiveness-parameter))
  (setf performance-time 0) ;;fix it!!
  
  (setf total-usability-value 
    (/ (+ learnability utility efficiency effectiveness) 4))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;     RETURN VALUE
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 1. Total Usability value
  ;; 2. LIST - Learnability, Memorability, Efficiency, Effectiveness
  ;; 3. Performance time
  ;; 4. Error value
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (list (floor total-usability-value)
	(list learnability utility efficiency effectiveness) 
	performance-time 
	(get-value-ACT-R-model-output))
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INPUT/OUTPUT PARAMETERS STRUCTURE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;set-input-parameters
(defstruct setting-input-parameter-list
  twin-age
  twin-gender
  twin-cognitive-ability
  twin-cognitive-focus-ability
  twin-physical-ability
  twin-physical-agility
  task-information
  twin-trajectory-timeline
  device-force-timeline
  event-reaction-time-list
  event-error-list
  task-performance-time
  task-performance-reserve
  twin-noise
  )

(defvar *SETTING-INPUT-PARAMETERS* (make-setting-input-parameter-list))

;;;set-output-variables
(defstruct setting-output-list
  ACT-R-output
  human-error
  mental-workload
  learnability
  memorability
  utility
  effectiveness
  total-usability
  )

(defvar *OUTPUT-PARAMETERS* (make-setting-output-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ** TWIN -> MODEL SYSTEM ** INPUT PARAMETER FUNCTIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; TWIN - PERSONAL INFORMATION
;;;

;;;
;;; TWIN AGE
;;;
(defun input-twin-age-parameter (value)
  (setf (setting-input-parameter-list-twin-age *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-twin-age ()
  (setting-input-parameter-list-twin-age *SETTING-INPUT-PARAMETERS*))


;;;
;;; TWIN GENDER
;;;
(defun input-twin-gender-parameter (value)
  (setf (setting-input-parameter-list-twin-gender *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-twin-gender ()
  (setting-input-parameter-list-twin-gender *SETTING-INPUT-PARAMETERS*))


;;;
;;; TWIN COGNITIVE ABILITY
;;;
(defun input-twin-cognitive-ability-parameter (value)
  (setf (setting-input-parameter-list-twin-cognitive-ability *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-twin-cognitive-ability ()
  (setting-input-parameter-list-twin-cognitive-ability *SETTING-INPUT-PARAMETERS*))


;;;
;;; TWIN COGNITIVE FOCUS ABILITY
;;;
(defun input-twin-cognitive-focus-ability-parameter (value)
  (setf (setting-input-parameter-list-twin-cognitive-focus-ability *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-twin-cognitive-focus-ability ()
  (setting-input-parameter-list-twin-cognitive-focus-ability *SETTING-INPUT-PARAMETERS*))


;;;
;;; TWIN PHYSICAL ABILITY (POWER)
;;;
(defun input-twin-physical-ability-parameter (value)
  (setf (setting-input-parameter-list-twin-physical-ability *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-twin-physical-ability ()
  (setting-input-parameter-list-twin-physical-ability *SETTING-INPUT-PARAMETERS*))


;;;
;;; TWIN PHYSICAL AGILITY (AGILITY, SPEED)
;;;
(defun input-twin-physical-agility-parameter (value)
  (setf (setting-input-parameter-list-twin-physical-agility *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-twin-physical-agility ()
  (setting-input-parameter-list-twin-physical-agility *SETTING-INPUT-PARAMETERS*))


;;;
;;; TWIN - TASK INFORMATION
;;;

;;;
;;; TASK INFORMATION
;;;
(defun input-task-information-parameter (value)
  (setf (setting-input-parameter-list-task-information *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-task-information ()
  (setting-input-parameter-list-task-information *SETTING-INPUT-PARAMETERS*))


;;;
;;; TWIN - TASK PERFORMANCE INFORMATION
;;;

;;;
;;; TASK RESULTS 1 (TRAJECTORY TIMELINE)
;;;
(defun input-twin-trajectory-timeline-parameter (value)
  (setf (setting-input-parameter-list-twin-trajectory-timeline *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-twin-trajectory-timeline ()
  (setting-input-parameter-list-twin-trajectory-timeline *SETTING-INPUT-PARAMETERS*))


;;;
;;; TASK RESULTS 2 (DEVICE FORCE TIMELINE)
;;;
(defun input-device-force-timeline-parameter (value)
  (setf (setting-input-parameter-list-device-force-timeline *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-device-force-timeline ()
  (setting-input-parameter-list-device-force-timeline *SETTING-INPUT-PARAMETERS*))


;;;
;;; TASK RESULTS - RESPONSE TIME PER TASK EVENT (LIST EVENT RESPONSE-TIME)
;;;
(defun input-event-reaction-time-list-parameter (value)
  (setf (setting-input-parameter-list-event-reaction-time-list *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-event-reaction-time-list ()
  (setting-input-parameter-list-event-reaction-time-list *SETTING-INPUT-PARAMETERS*))


;;;
;;; TASK RESULTS - ERROR PER TASK EVENT (LIST EVENT ERROR)
;;;
(defun input-event-error-list-parameter (value)
  (setf (setting-input-parameter-list-event-error-list *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-event-error-list ()
  (setting-input-parameter-list-event-error-list *SETTING-INPUT-PARAMETERS*))


;;;
;;; TASK RESULTS - PERFORMANCE TIME
;;;
(defun input-task-performance-time-parameter (value)
  (setf (setting-input-parameter-list-task-performance-time *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-task-performance-time ()
  (setting-input-parameter-list-task-performance-time *SETTING-INPUT-PARAMETERS*))


;;;
;;; RESERVE PARAMETER
;;;
(defun input-task-performance-reserve-parameter (value)
  (setf (setting-input-parameter-list-task-performance-reserve *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-task-performance-reserve ()
  (setting-input-parameter-list-task-performance-reserve *SETTING-INPUT-PARAMETERS*))


;;;
;;; TWIN NOISE PARAMETER
;;;
(defun input-twin-noise-parameter (value)
  (setf (setting-input-parameter-list-twin-noise *SETTING-INPUT-PARAMETERS*) value))

(defun get-value-twin-noise ()
  (setting-input-parameter-list-twin-noise *SETTING-INPUT-PARAMETERS*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ** MODEL -> TWIN SYSTEM ** OUTPUT PARAMETER FUNCTIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; ACT-R MODEL OUTPUT
;;;
(defun input-ACT-R-model-output (value)
  (setf (setting-output-list-ACT-R-output *OUTPUT-PARAMETERS*) value))

(defun get-value-ACT-R-model-output ()
  (setting-output-list-ACT-R-output *OUTPUT-PARAMETERS*))


;;;
;;; HUMAN ERROR
;;;
(defun input-human-error-parameter (value)
  (setf (setting-output-list-human-error *OUTPUT-PARAMETERS*) value))

(defun get-value-human-error-parameter ()
  (setting-output-list-human-error *OUTPUT-PARAMETERS*))


;;;
;;; MENTAL WORKLOAD
;;;
(defun input-mental-workload-parameter (value)
  (setf (setting-output-list-mental-workload *OUTPUT-PARAMETERS*) value))

(defun get-value-mental-workload-parameter ()
  (setting-output-list-mental-workload *OUTPUT-PARAMETERS*))


;;;
;;; LEARNABILITY
;;;
(defun input-learnability-parameter (value)
  (setf (setting-output-list-learnability *OUTPUT-PARAMETERS*) value))

(defun get-value-learnability-parameter ()
  (setting-output-list-learnability *OUTPUT-PARAMETERS*))


;;;
;;; MEMORABILITY
;;;
(defun input-memorability-parameter (value)
  (setf (setting-output-list-memorability *OUTPUT-PARAMETERS*) value))

(defun get-value-memorability-parameter ()
  (setting-output-list-memorability *OUTPUT-PARAMETERS*))


;;;
;;; UTILITY
;;;
(defun input-utility-parameter (value)
  (setf (setting-output-list-utility *OUTPUT-PARAMETERS*) value))

(defun get-value-utility-parameter ()
  (setting-output-list-utility *OUTPUT-PARAMETERS*))


;;;
;;; EFFECTIVENESS
;;;
(defun input-effectiveness-parameter (value)
  (setf (setting-output-list-utility *OUTPUT-PARAMETERS*) value))

(defun get-value-effectiveness-parameter ()
  (setting-output-list-utility *OUTPUT-PARAMETERS*))


;;;
;;; TOTAL USABILITY VALUE
;;;
(defun input-total-usability-parameter (value)
  (setf (setting-output-list-total-usability *OUTPUT-PARAMETERS*) value))

(defun get-value-total-usability-parameter ()
  (setting-output-list-total-usability *OUTPUT-PARAMETERS*))

