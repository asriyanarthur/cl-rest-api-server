(defpackage :my-server
  (:use :common-lisp))

(in-package :my-server)

;;(proclaim '(optimize (debug 3)))
;;(declaim (optimize (debug 2))
;;(declaim (optimize (speed 0) (space 0) (debug 3) (safety 0)))

;;(ql:quickload :ningle)
;;(ql:quickload :clack)
;;(ql:quickload :cl-json)
;;(ql:quickload :local-time)
;;(ql:quickload :parse-number)

(defun zmy-date-converter-in (date-string)
  "convert date from format |dd*mm*yyyy| to timestamp - * - any one character"
  (let ((day (subseq date-string 1 3))
	(month (subseq date-string 4 6))
	(year (subseq date-string 7 11)))
    (ignore-errors (local-time:parse-timestring (concatenate 'string year "-" month "-" day)))))

(defun zmy-date-converter-out (local-date)
  "convert timestamp into format dd.mm.yyyy"
  (let ((local-date-string (write-to-string local-date)))
    (concatenate 'string
		 (subseq local-date-string 9 11)
		 "."
		 (subseq local-date-string 6 8)
		 "."
		 (subseq local-date-string 1 5))))

(defun zmy-get-timestamp-with-month-offset (local-date num-offset)
  (if (not local-date) (return-from zmy-get-timestamp-with-month-offset nil))
  (local-time:adjust-timestamp local-date (offset :month num-offset)))

(defun round-to-ratio (number precision &optional (round-function #'round))
  "Округляет число NUMBER до количества знаков PRECISION, если PRECISION
отрицательно, то происходит потеря разрядов. Возвращает рациональное число,
ROUND-FUNCTION - округляющая функция, например:
  #'floor
  #'ceil
  #'truncate
  #'round (по-умолчанию)."
  (let ((div (expt 10 precision)))
    (/ (funcall round-function (* number div)) div)))

(defun round-to-float (number precision &optional (round-function #'round))
  "Округляет число NUMBER до количества знаков PRECISION, если PRECISION
отрицательно, то происходит потеря разрядов. Возвращает дробное число.
ROUND-FUNCTION - округляющая функция, например:
  #'floor
  #'ceil
  #'truncate
  #'round (по-умолчанию)."
  (float (round-to-ratio number precision round-function)))

(defun zmy-validation (zdate zrate zperiods zamount)
  "check params, if T - then error"
      (if (or (not (numberp zrate))
	      (not (numberp zperiods))
	      (not (numberp zamount))
	      (eq zdate nil)
	      (eq zrate nil)
	      (eq zperiods nil)
	      (eq zamount nil)
	      (> zrate 8)
	      (< zrate 1)
	      (< zamount 10000)
	      (> zamount 3000000)
	      (> zperiods 60)
	      (< zperiods 1)) 
	  t nil))

(defun mdeposit-pre (params)
  (let ((zdate (write-to-string (cdr (assoc "date" params :test #'string=))))
	(zrate (cdr (assoc "rate" params :test #'string=)))
	(zperiods (cdr (assoc "periods" params :test #'string=)))
	(zamount (cdr (assoc "amount" params :test #'string=)))
	(loop-res '())
	(loop-res2 '()))
    (if (zmy-validation zdate zrate zperiods zamount) (return-from mdeposit-pre loop-res2)) 
    (let ((ztimestamp (zmy-date-converter-in zdate))
	  (cumm-amount zamount))
      (if (not ztimestamp) (return-from mdeposit-pre nil))
      (loop :for x :from 0 :to (- zperiods 1) :do
	   (push (zmy-get-timestamp-with-month-offset ztimestamp x) loop-res))
      (setf loop-res (nreverse loop-res))
      (loop :for y :in loop-res :do
	   (setf cumm-amount (* cumm-amount (+ 1 (/ zrate 12 100))))
	   (push (cons (zmy-date-converter-out y) (round-to-float cumm-amount 2)) loop-res2)))
    loop-res2))

(defun mdeposit (params)
  (let ((loop-res2 (mdeposit-pre params))
	(loop-res3 '()))
    (push "{" loop-res3)
    (cond ((eq loop-res2 nil) 
	   `(400 (:content-type "application/json") ("{\"error\":\"description of error in my taste\"}")))
	  (t (loop :for x :in (nreverse loop-res2) :do
		(push (concatenate 'string
				   "\""
				   (car x)
				   "\": "
				   (write-to-string (cdr x))
				   ",")
		      loop-res3)
		  :finally (setf (car loop-res3) (substitute #\SPACE #\, (car loop-res3))))
	     (push "}" loop-res3)	      
	     `(200 (:content-type "application/json") ,(nreverse loop-res3))
	     ))))

(defun adeposit (params)
  (let ((loop-res2 (mdeposit-pre params)))
    (let ((last-sum (cdr (car loop-res2))) 
	  (init-sum (cdr (assoc "amount" params :test #'string=)))) 
      (cond ((eq loop-res2 nil)
	     `(400 (:content-type "application/json") ("{ \"error\":\" description of error in my taste\"}")))
	     (t `(200 (:content-type "application/json") (,(concatenate 'string
							     "{ \"profit\": "
							     (write-to-string (round-to-float (- last-sum init-sum) 2))
							     "}"))))))))
      
(defvar *app* (make-instance 'ningle:app))

(defun my-server-start()
  "Register routes and start server"
  (setf (ningle:route *app* "/mdeposit" :method :POST)
	#'(lambda (params)
	    (mdeposit params)))  
  (setf (ningle:route *app* "/adeposit" :method :POST)
	#'(lambda (params)
	    (adeposit params)))  
  (clack:clackup *app* :port 8080 :address "0.0.0.0")  
  (loop :do (sleep 5)))

(defun test111()
    (let (( zzz '(("rate" . 3.7) ("amount" . 450000) ("periods" . 5) ("date" . 31.12.2020))))
      (mdeposit zzz)))

;;(defun test222()
;;    (let (( zzz '(("rate" . 3.5) ("amount" . 10000) ("periods" . 0) ("date" . 01.01.2019))))
;;      (adeposit zzz)))

;;(defun test333()
;;  (zmy-validation "01.01.2020" 10 1 500000))

;;(defun test222()
;;    (zmy-validation "|asdas.13.2019|" 3 5 120000))

;;#+sb-core-compression
;;(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
;;  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
