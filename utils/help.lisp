(defstruct (command (:print-function print-command))
  form
  short
  long)

(defmethod print-object ((object command) stream)
  (format stream "~A" (command-long object)))

(defun print-short (cmd)
  (format nil "~10,,+2@A - ~A~%" (command-form cmd) (command-short cmd)))

(defvar help-ld (make-command
		 :form "(ld <num>)"
		 :short "Loads day <num> solutions & methods into the REPL"
		 :long (format nil
			       (concatenate 'string
					    "(ld <num>) - Loads the day <num> file and runs the parse and solution functions, "
					    "when loaded, the results can be seen by typing day<num> into the REPL.~%"
					    "Also sets the loaded-day variable to the given day."))))

(defvar help-load-all (make-command
		       :form "(load-all)"
		       :short "Loads solutions for all days (even ones not completed)"
		       :long (format nil
				     (concatenate 'string
						  "(load-all) - Loads all day files and runs their parsers and solutions."
						  " When complete, any day's results can be seen by typing day<num> into the REPL."))))

(defvar help-test (make-command
		   :form "(test &args)"
		   :short "Tests a function with given args"
		   :long (format nil
				 (concatenate 'string
					      "(test &args) - Tests a function with arguments supplied. If no arguments are given, "
					      "it will run the last function tested with the arguments it was tested with. "
					      "Otherwise the first argument is the function name and the rest are the arguments "
					      "that function uses."))))

(defvar help-test-days (make-command
			:form "(test-days <days>)"
			:short "Shows the results for the example inputs for the given days"
			:long (format nil
				      (concatenate 'string
						   "(test-days <days>) - Runs the solutions on the example input for each given day "
						   "and displays them alongside the expected results.~%"
						   "The days need to be loaded in the REPL and are given in the form: day<num>. "
						   "If no days are specified, the current loaded day will be used."))))

(defvar help-test-all-days (make-command
			    :form "(test-all-days)"
			    :short "Tests all loaded days"
			    :long (format nil
					  (concatenate 'string
						       "(test-all-days) - Runs the solutions on the example inputs for all days "
						       "that have been loaded into the REPL and displays them alongside "
						       "the expected results"))))

(defvar help-parse-in (make-command
		       :form "(parse-in <day>)"
		       :short "Shows the parsed input for the given day"
		       :long (format nil
				     (concatenate 'string
						  "(parse-in <day>) - Returns the parsed input for <day> "
						  "input should be a .txt file in the input folder "
						  "<day> is a number"))))

(defvar help-reload (make-command
		     :form "(reload)"
		     :short "Reloads the currently loaded day"
		     :long "Reloads the currently loaded day, updating functions, variables etc."))

(defvar help-reload-all (make-command
			 :form "(reload-all)"
			 :short "Reloads all loaded days"
			 :long "Reloads all loaded days, updating functions, variables etc."))

(defvar help-bench (make-command
		    :form "(bench <day> <function> <args>)"
		    :short "Records the time taken to perform a given day/function"
		    :long (format nil
				  (concatenate 'string
					       "(bench <day> <function> <args>) - Records the time taken "
					       "to perform a given day/function. <day> and <function> are optional; if "
					       "<day> isn't specified, then it will use the currently loaded day, if "
					       "<function> is not specified, then it will benchmark: parsing the input, "
					       "part a, and part b. If a function is given, then a day must also be given. "
					       "If a <function> requires arguments they are given "
					       "in <args> seperated by a space."))))



(defstruct (help (:print-function print-help))
  ld
  load-all
  test
  test-days
  test-all-days
  parse-in
  reload
  reload-all
  bench)

(defvar commands (list help-ld help-load-all help-test help-test-days
		       help-test-all-days help-parse-in help-reload help-reload-all help-bench))

(defmethod print-object ((object help) stream)
  (format stream
	  (concatenate 'string
		       "A template structure for Advent of Code solutions written in Common Lisp.~%"
		       "Each day can be loaded and run in the REPL, "
		       "as well as checking the example input produces the desired result.~%"
		       "Functions:~%"
		       "~{~A~}"
		       "For more details on a function type help-<function>~%")
	  (mapcar 'print-short commands)))

(defvar help)
(setq help (make-help
	    :ld help-ld
	    :load-all help-load-all
	    :test help-test
	    :test-days help-test-days
	    :test-all-days help-test-all-days
	    :parse-in help-parse-in
	    :reload help-reload
	    :reload-all help-reload-all
	    :bench help-bench))

(defun help () help)
