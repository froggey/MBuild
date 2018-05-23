(defpackage :clim-lookup (:use :common-lisp :split-sequence)
            (:export :term-lookup :populate-table))
(in-package :clim-lookup)

(defvar *clim-table*)

(defvar *clim-abbrev-table*)

(defparameter *clim-file*
  (merge-pathnames "mrindex"
                   (make-pathname
                    :directory
                    (pathname-directory
                     (or *load-truename*
                         *default-pathname-defaults*)))))

(defun merge-to-spec (url)
  (concatenate 'string "http://www.stud.uni-karlsruhe.de/~unk6/clim-spec/"
               url))

(defun set-abbrev (term)
  (let ((abbrev (abbrev:abbrev term)))
    (if abbrev
        (pushnew term (gethash abbrev *clim-abbrev-table* nil)
                 :test #'string-equal))))

(defun populate-table ()
  (setf *clim-table* (make-hash-table :test #'equalp))
  (setf *clim-abbrev-table* (make-hash-table :test #'equalp))
  (with-open-file (f *clim-file* :direction :input)
    (loop for i = (read f nil nil)
          while i
          do (destructuring-bind (ig1 (term sep (ig2 type)) url)
                 i
               (declare (ignore ig1 ig2 sep))
               (setf term (substitute #\space (code-char 160) term :test #'eql))
               (setf type (substitute #\space (code-char 160) type :test #'eql))
               (push (cons type url)
                     (gethash term *clim-table* nil))
               (set-abbrev term)))))

(defun abbrev-lookup (term)
  (let ((found (gethash term *clim-abbrev-table* nil)))
    (if found
        (if (eql (length found) 1)
            (let ((r (real-term-lookup (car found))))
              (and r
                   (concatenate 'string (car found) ": " r)))
            (format nil "Multiple matches found. Try any of: ~{~A~^ ~}"
                    found)))))

(defun real-term-lookup (term)
  (destructuring-bind (real-term &optional type (index-str "0"))
      (split-sequence #\, term)
    (let ((ents (gethash real-term *clim-table* nil))
          (index (parse-integer index-str :junk-allowed t)))
      (if type
          (let ((all-type (loop for ent in ents
                                if (string-equal (car ent) type)
                                collect ent)))
            (if (< index (length all-type))
                (merge-to-spec (cdr (nth index all-type)))
                (format nil "Invalid index ~A: must be between 0 and ~A."
                        index (1- (length all-type)))))
          (if (eql (length ents) 0)
              nil
              (if (eql (length ents) 1)
                  (merge-to-spec (cdr (car ents)))
                  (let ((unique-types nil))
                    (loop for ent in ents
                          do (pushnew (car ent) unique-types :test #'string-equal))
                    (format nil "Multiple entries found. Try looking up one of: ~{\"~A\"~^, ~}"
                            (mapcar #'(lambda (type)
                                        (format nil "~A,~A~A"
                                                real-term
                                                type
                                                (let ((count (count type ents :key #'car :test #'string-equal)))
                                                  (if (> count 1)
                                                      (format nil ",{0-~A}"
                                                              (1- count))
                                                      ""))))
                                    unique-types)))))))))

(defun term-lookup (term)
  (or (real-term-lookup term)
      (abbrev-lookup term)))
