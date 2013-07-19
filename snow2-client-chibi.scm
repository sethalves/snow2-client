#!/usr/local/bin/chibi-scheme

(import (scheme base))
(import (scheme read))
(import (scheme write))
(import (chibi io))
(import (chibi process))
(import (chibi pathname))
(import (scheme load))
(import (scheme process-context))
(import (srfi 1))
(import (srfi 69))
(import (scheme file))
(import (chibi filesystem))

(define program-and-command-line command-line)

;; (include "snow2-client-common.scm")



(define (report-error format-string . args)
  (display format-string)
  (newline)
  (display args)
  (newline)
  #f)


(define (with-input-from-request uri writer-thunk reader-thunk)
  #t)

(define (untar filename)
  ;; (let ((tar-pid (process-run (format #f "tar xf '~A'" filename))))
  ;;   (process-wait tar-pid))
  #t
  )


filter



(define top (path-directory (car (command-line))))
(load (string-append top "/snow2-client-common.scm"))
(main-program)
