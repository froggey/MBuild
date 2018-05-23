(defpackage :steel-bazooka (:use :cl) (:export :steel-whatever))
(in-package :steel-bazooka)

(defparameter *words* nil)

(defun read-words ()
  (loop for char = #\a then (code-char (1+ (char-code char)))
       while (char<= char #\z)
       do (push (cons char (make-array '(0) :adjustable t :fill-pointer 0 :element-type 'string)) *words*))
  (with-open-file (f (merge-pathnames "words"
				      (make-pathname :directory (pathname-directory
								 (load-time-value *load-truename*)))) :direction :input)
    (loop for line = (read-line f nil nil)
	 while line
       if (> (length line) 2)
       do (if (char<= #\a (elt line 0) #\z)
	      (vector-push-extend line (cdr (assoc (elt line 0) *words* :test #'eql)))))))

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun steel-whatever (&key (letters "sb") (suffix " Common Lisp"))
  (format nil "~:(~{~A~^ ~}~)~A"
	  (map 'list
	       (lambda (letter)
		 (random-elt (cdr (assoc letter *words* :test #'eql))))
	       letters)
	  (if suffix suffix "")))

(read-words)