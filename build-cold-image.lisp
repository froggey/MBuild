;;;; Build the cold image

#-quicklisp
(error "Quicklisp not found!")

#-sb-unicode
(error "Unicode support is required.")

;; The cold-generator requires a distinct (vector (unsigned-byte 64)) type.
#-(or 64-bit arm64 x86-64 ppc64)
(error "A 64-bit SBCL is required.")

(setf sb-impl::*default-external-format* :utf-8)

(format t "Loading build prerequisites....~%")
#+quicklisp
(ql:quickload '(:nibbles :cl-ppcre :iterate :alexandria :closer-mop :trivial-gray-streams))
#-quicklisp
(progn
  (require :asdf)
  (require :nibbles)
  (require :cl-ppcre)
  (require :iterate)
  (require :alexandria)
  (require :closer-mop)
  (require :trivial-gray-streams))

(format t "Loading Mezzano compiler and cold-generator....~%")
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :lispos)
;; Initialize the compiler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Choose your architecture here.
;;
;; :x86-64 is well supported and the standard target.
;; :arm64 is a secondary target, may not be functional and has many missing features.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cold-generator:set-up-cross-compiler :architecture :x86-64)

(format t "Building cold image...~%")
(cold-generator::make-image "../../mezzano" :image-size (* 5 1024 1024 1024) :header-path "tools/disk-header.bin")

(format t "Build successful!~%")
(quit)
