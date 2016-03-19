;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

#-quicklisp
(error "Quicklisp not found!")

(setf sb-impl::*default-external-format* :utf-8)

(format t "Loading build prerequisites....~%")
#+quicklisp
(ql:quickload '(:nibbles :cl-ppcre :iterate :alexandria))
#-quicklisp
(progn
  (require :asdf)
  (require :nibbles)
  (require :cl-ppcre)
  (require :iterate)
  (require :alexandria))

(format t "Loading Mezzano compiler and cold-generator....~%")
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)
;; Initialize the compiler.
(with-compilation-unit ()
  (sys.c::set-up-cross-compiler)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*supervisor-source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*warm-source-files*))

(format t "Building cold image...~%")
(cold-generator::make-image "../mezzano" :image-size (* 512 1024 1024) :header-path "tools/disk-header.bin")

(format t "Build successful!~%")
(quit)
