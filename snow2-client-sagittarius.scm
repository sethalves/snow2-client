#! /bin/sh
#| -*- scheme -*-
exec sash -L . -S .sld $0 "$@"
|#

(import (scheme base) (scheme read) (scheme write) (scheme process-context))
(import (prefix (seth snow2-utils) snow2-))
(import (seth string-read-write) (seth srfi-27-random))


(define (usage pargs)
  (display (car pargs))
  (display " ")
  (display "<operation> '(library name)'")
  (newline)
  (display "  <operation> can be \"install\" or \"uninstall\"")
  (newline))


(define (main-program)
  (random-source-randomize! default-random-source)
  (let* ((repository-url
          "http://snow2.s3-website-us-east-1.amazonaws.com/")
         (repository (snow2-get-repository repository-url))
         (pargs (command-line)))

    ;; (display "repository=")
    ;; (display repository)
    ;; (newline)

    (cond ((not (= (length pargs) 3))
           (usage pargs))
          (else
           (let ((operation (list-ref pargs 1))
                 (library-name (read-from-string (list-ref pargs 2))))
             (cond ((equal? operation "install")
                    (snow2-install repository library-name))
                   ((equal? operation "uninstall")
                    (snow2-uninstall repository library-name))
                   ))))))


(main-program)
