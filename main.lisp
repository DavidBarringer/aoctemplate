(setf sb-ext:*muffled-warnings* 'style-warning)

(load "utils/preload.lisp")
(load "utils/help.lisp")
(defvar test-sol-a)
(defvar test-sol-b)

(defparameter *successful-load*
  (format nil "~%~%Advent of Code template for Common Lisp loaded!~%~%"))

(defstruct (day (:print-function print-day))
  number
  test-input
  input-file
  parsed-input
  part-a-res
  part-a-test
  part-b-res
  part-b-test
  test-sol-a
  test-sol-b)

(defmethod print-object ((object day) stream)
  (format stream "~%Day ~A:~%Test input file path: ~A~%Input file path: ~A~%Part a solution: ~A~%Part b solution: ~A~%"
          (day-number object)
          (write-to-string (day-test-input object))
          (write-to-string (day-input-file object))
          (if (null (day-part-a-res object)) (write-to-string "Part a not complete yet") (eval (day-part-a-res object)))
          (if (null (day-part-b-res object)) (write-to-string "Part b not complete yet") (eval (day-part-b-res object)))))

(defvar loaded-day nil)
(defvar loaded-days nil)
(defvar testing-function nil)
(defvar function-args (make-hash-table))


(defun get-file-lines (name)
  ;;; Function used to load data from a file, seperated by new line, no other parsing happens here
  (uiop:read-file-lines name))

(defun get-file-string (name)
  ;;; Function used to load data from a file as a single string
  (remove #\Return (uiop:read-file-string name)))

(defun ld-sort (day1 day2)
  (cond ((= (length (format nil "~A" day1)) (length (format nil "~A" day2)))
	 (string< (format nil "~A" day1) (format nil "~A" day2)))
        (t
	 (< (length (format nil "~A" day1)) (length (format nil "~A" day2))))))

(defmacro ld (day-num)
  ;;; Used to load a specific day
  (declaim #+sbcl(sb-ext:muffle-conditions style-warning))
  (load (format nil "days/day~2,'0D.lisp" (eval day-num)))
  (declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))
  (setf loaded-day (read-from-string (format nil "day~D" day-num)))
  (setf loaded-days (sort (remove-duplicates (cons loaded-day loaded-days)) 'ld-sort))
  `(progn (defvar ,loaded-day)
          (setq ,loaded-day (make-day :number ,day-num
                                      :test-input (format nil "input/test/~2,'0D.txt" ,day-num)
                                      :input-file (format nil "input/~2,'0D.txt" ,day-num)
                                      :parsed-input (parse-input (format nil "input/~2,'0D.txt" ,day-num))
                                      :part-a-res `(part-a (parse-input (day-input-file ,loaded-day)))
                                      :part-a-test `(part-a (parse-input (day-test-input ,loaded-day)))
                                      :part-b-res `(part-b (parse-input (day-input-file ,loaded-day)))
                                      :part-b-test `(part-b (parse-input (day-test-input ,loaded-day)))
                                      :test-sol-a test-sol-a
                                      :test-sol-b test-sol-b))
          loaded-day))

(defun test (&rest args)
  (cond ((AND (null args) (NOT (null testing-function)))
	 (apply testing-function (gethash testing-function function-args)))
	((null args) nil)
	(t (setf (gethash (CAR args) function-args) (CDR args))
	   (setf testing-function (CAR args))
	   (test))))

(defun test-print (values)
  (format *standard-output*
	  "~{~%~%Day ~A:~%Part A:~% Expected: ~A~% Actual:   ~A~%~%Part B:~% Expected: ~A~% Actual:   ~A~%~%~}"
	  values))

(defun test-days (&rest days)
  (cond ((AND (null days) (null loaded-day)) (error "Test called, but no days were loaded or specified."))
        ((null days) (test-print (list (day-number (eval loaded-day)) (day-test-sol-a (eval loaded-day))
                                       (eval (day-part-a-test (eval loaded-day))) (day-test-sol-b (eval loaded-day))
                                       (eval (day-part-b-test (eval loaded-day))))))
        (t (test-print
	    (loop for d in days for day = (eval d)
		  nconc (list
			 (day-number day)
			 (day-test-sol-a day)
			 (day-part-a-test day)
			 (day-test-sol-b day)
			 (day-part-b-test day)))))))

(defun test-all-days ()
  (apply 'test loaded-days))

(defun parse-in (day)
  (day-parsed-input day))

(defmacro load-all ()
  (let ((files (mapcar (lambda (f) (CAR (last (split "/" (namestring f))))) (uiop:directory-files "./days/"))))
    `(load-all-prime ,(mapcar (lambda (s) (parse-integer (subseq s 3) :junk-allowed t)) files))))

(defmacro load-all-prime (days)
  (cond ((null days) days)
        (t `(cons (ld ,(eval (CAR days))) (load-all-prime ,(CDR days))))))

(defmacro reload ()
  `(ld ,(eval (day-number (eval loaded-day)))))

(defmacro reload-all ()
  `(reload-prime ,loaded-days))

(defmacro reload-prime (days)
  (cond ((null days) nil)
        (t `(cons (ld ,(eval (day-number (eval (CAR days))))) (reload-prime ,(CDR days))))))

;;; Benchmarking tools

(defun print-bench (details result)
  ;;; This function prints
  (format t "~A runs in: ~Fs~%" details (float result)))

(defun bench (&optional day func-name &rest args)
  (if (null day) nil (setf loaded-day (read-from-string (format nil "day~A" day))))
  (declaim #+sbcl(sb-ext:muffle-conditions style-warning))
  (load (format nil "days/day~2,'0D.lisp" (day-number (eval loaded-day))))
  (declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))
  (cond ((null func-name)
	 (print-bench (format nil "Day ~D (all parts)" (day-number (eval loaded-day)))
		      (+ (bench-fraction (format nil "Day ~D parsing input" (day-number (eval loaded-day)))
					 'parse-input
					 (day-input-file (eval loaded-day)))
			 (bench-fraction (format nil "Day ~D part a" (day-number (eval loaded-day)))
					 'part-a
					 (day-parsed-input (eval loaded-day)))
			 (bench-fraction (format nil "Day ~D part b" (day-number (eval loaded-day)))
					 'part-b
					 (day-parsed-input (eval loaded-day))))))
        ((eq func-name 'a)
	 (bench-fraction (format nil "Day ~D part a" (day-number (eval loaded-day)))
			 'part-a
			 (day-parsed-input (eval loaded-day)))
	 (format t ""))
        ((eq func-name 'part-a)
	 (bench-fraction (format nil "Day ~D part a" (day-number (eval loaded-day)))
						'part-a
						(day-parsed-input (eval loaded-day)))
	 (format t ""))
        ((eq func-name 'b)
	 (bench-fraction (format nil "Day ~D part b" (day-number (eval loaded-day)))
					   'part-b
					   (day-parsed-input (eval loaded-day)))
	 (format t ""))
        ((eq func-name 'part-b)
	 (bench-fraction
	  (format nil "Day ~D part b" (day-number (eval loaded-day)))
						'part-b
						(day-parsed-input (eval loaded-day)))
	 (format t ""))
        ((eq func-name 'parse)
	 (bench-fraction
	  (format nil "Day ~D parsing input" (day-number (eval loaded-day)))
					       'parse-input
					       (day-input-file (eval loaded-day)))
	 (format t ""))
        ((eq func-name 'parse-input)
	 (bench-fraction
	  (format nil "Day ~D parsing input" (day-number (eval loaded-day)))
			 'parse-input
			 (day-input-file (eval loaded-day)))
	 (format t ""))
        (t (bench-fraction
	    (format nil "Day ~D function ~A" (day-number (eval loaded-day)) func-name args))
	   (format t ""))))

(defun bench-fraction (details func-name &rest args)
  ;;; This function runs file given amount of times and averages the real times
  (let ((t1 (get-internal-real-time)))
    (apply func-name args)
    (let ((t2 (get-internal-real-time)))
      (print-bench details (/ (- t2 t1) internal-time-units-per-second))
      (/ (- t2 t1) internal-time-units-per-second))))
(setf sb-ext:*muffled-warnings* nil)

(format t *successful-load*)
