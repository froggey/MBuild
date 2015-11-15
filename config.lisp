;; This form is spliced into ipl.lisp, so the progn must cover the entire file.
(progn
  ;; This must be set to the LAN IP address of the computer running the file-server.
  (defparameter *file-server-ip* '(192 168 0 123))
  ;; Full path to the Mezzano-build directory (the directory containing this file).
  ;; This must be a string, not a #p"pathname" due to cross-compiler issues.
  ;; It must also end in a slash /.
  (defparameter *build-directory* "/The/full/path/to/Mezzano-build/")
  )
