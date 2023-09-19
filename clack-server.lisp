(ql:quickload :clack)

; (defun handler (env)
;   (declare (ignore env))
;   '(200 (:content-type "text/plain")
;     ("Hello, Clack!")
;     ))

;; 1. Total Usability value
;; 2. LIST - Learnability, Memorability, Efficiency, Effectiveness
;; 3. Performance time
;; 4. Error value

(defun handler (env)
  (declare (ignore env))
  (setf result (run-the-model-until-time 10 (list "arm" "adaptive") 39 "high" nil 20 5))
  (setf total-usability-value (car result))
  (setf learnability (caadr result))
  (setf memorability (cadadr result))
  (setf efficiency (car (cdr (cdr (car (cdr result))))))
  (setf effectiveness (car (cdr (cdr (cdr (car (cdr result)))))))
  (setf performance-time (caddr result))
  (setf error-value (cadddr result))
  `(200
       (:content-type "text/plain")
         (,(format nil "~S ~S ~S ~S ~S ~S ~S" 
                   total-usability-value
                   learnability
                   memorability
                   efficiency
                   effectiveness
                   performance-time
                   error-value
                   ))))

; (defun handler (env)
;   (destructuring-bind (&key request-method path-info request-uri
;                             query-string headers &allow-other-keys)
;       env
;     `(200
;       nil
;       (,(format nil "Method: ~S Path: ~S URI: ~A Query: ~S~%Headers: ~S"
;                 request-method path-info request-uri query-string
;                 (alexandria:hash-table-alist headers))))))

(defvar *clack-server*
  (clack:clackup (lambda (env) (funcall 'handler env))))
