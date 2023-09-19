(ql:quickload '(:clack :ningle :djula :dexador :cl-json))

;(djula:add-template-directory  #P"templates/")
(djula:add-template-directory  #P"web/templates/")
(defparameter *template-registry* (make-hash-table :test 'equal))

;; render template - copied & modified from caveman
(defun render (template-path &optional data)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template* template nil data)))

(defvar *app* (make-instance 'ningle:app))

(djula:def-filter :truncate-desc (val)
  (if (> (length val) 100)
      (concatenate 'string (subseq val 0 97) "...")
      val))

(defun increment-page (page)
  (1+ (parse-integer page)))

;; GET /
(setf (ningle:route *app* "/")
      #'(lambda (params)
      (render #P"index.html")))

;; GET /usability.html
(setf (ningle:route *app* "/usability.html")
      #'(lambda (params)
    (setf result (run-the-model-until-time 10 (list "arm" "adaptive") 39 "high" nil 20 5))
	  (let ((total-usability-value (car result))
          (learnability (caadr result))
          (memorability (cadadr result))
          (efficiency (car (cdr (cdr (car (cdr result))))))
          (effectiveness (car (cdr (cdr (cdr (car (cdr result)))))))
          (performance-time (caddr result))
          (error-value (cadddr result))
          )
      (render #P"usability.html" (list :total-usability-value total-usability-value 
                                       :learnability learnability
                                       :memorability memorability
                                       :efficiency efficiency
                                       :effectiveness effectiveness
                                       :performance-time performance-time
                                       :error-value error-value)))))

;; GET /usability
(setf (ningle:route *app* "/usability")
      (lambda (params)
        (setf result (run-the-model-until-time 10 (list "arm" "adaptive") 39 "high" nil 20 5))
    	  (let ((total-usability-value (car result))
          (learnability (caadr result))
          (memorability (cadadr result))
          (efficiency (car (cdr (cdr (car (cdr result))))))
          (effectiveness (car (cdr (cdr (cdr (car (cdr result)))))))
          (performance-time (caddr result))
          (error-value (cadddr result))
          )
        (declare (ignore params))
          (format nil "{\"usability\":~S, \"learnability\":~S,\"memorability\":~S, \"efficiency\":~S, \"effectiveness\":~S, \"performance\":~S}"
                  total-usability-value learnability memorability efficiency effectiveness performance-time)
        )))

(clack:clackup
           *app*
           :port 5050)
