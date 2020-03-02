;; the * means a global variable in lisp
(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  ;; list, function that makes a list with the arguments
  ;; this makes a property list (plist)
  ;; for now think of it as an associative array
  ;; ex: cd["title"] => urban flora ep
  (list :title title :artist artist :rating rating :ripped ripped))

;; push is a macro in common lisp
(defun add-record (cd) (push cd *db*))

;; print out the db
;; ~a, consume an arugment, drops leading ':' and '"'
;; ~10t, tabulate to the 10th column
;; ~{ ... ~}, inside must be a list
;; ~%, newline
(defun dump-db ()
	(format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  ;; *query-io*, global variable that contains
  ;; the input stream to the terminal
  (format *query-io* "~a: " prompt)
  ;; needed because some impl of lisp will wait
  ;; for a newline before printing the prompt
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   ;; parse-integer throws an error if it can't
   ;; parse an integer from the string.
   ;; :junk-allowed relaxes it somewhat
   ;; or, lisp macro. similar to short circuiting '||' in C++
   ;; if an integer can't be read then 0 will returned
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   ;; y-or-n-p, function that reads Y,y,N,n if it's not that
   ;; it will redisplay the prompt
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  ;; loop, macro that repeats expressiots until RETURN is called
  (loop (add-record (prompt-for-cd))
	 (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
					   :direction :output
					   :if-exists  :supersede)
	(with-standard-io-syntax
		(print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax
	  ;;setf is assignment operator in CL
	  (setf *db* (read in)))))

;; the form for lambdas is
;; (lambda (parameters) (body))
(defun select-by-artist (artist)
  (remove-if-not
   ;; the '#' gets the function with the name after the '#'
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisions-list (fields)
  (loop while fields
	   collecting (make-comparison-expr (pop fields) (pop fields))))

;; &key assigns values to the specified parameters
;; ex: (where(:artist "alina baraz"))
;; will assign "alina baraz" to the 'artist' paramater
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisions-list clauses))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
		(mapcar
		 #'(lambda (row)
			 (when (funcall selector-fn row)
	           (if title    (setf (getf row :title)  title))
	           (if artist   (setf (getf row :artist) artist))
	           (if rating   (setf (getf row :rating) rating))
	           (if ripped-p (setf (getf row :ripped) ripped)))
			 row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
