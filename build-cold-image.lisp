#-quicklisp
(error "Quicklisp not found!")

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
;; Patch up *warm-objects* to use the custom IPL file instead of the stock one.
(nsubst "../ipl.lisp" "ipl.lisp" cold-generator::*warm-source-files* :test 'equal)
;; Initialize the compiler.
(with-compilation-unit ()
  (sys.c::set-up-cross-compiler)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*supervisor-source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*source-files*)
  (mapc 'sys.c::load-for-cross-compiler cold-generator::*warm-source-files*))

(format t "Building cold image...~%")
(cold-generator::make-image "../mezzano" :image-size (* 512 1024 1024) :header-path "tools/disk_header")

(format t "Build successful!~%")
(quit)
