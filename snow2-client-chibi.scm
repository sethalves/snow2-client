#! /bin/sh
#| -*- scheme -*-
exec chibi-scheme -I /usr/local/share/scheme -s $0 "$@"
|#

(import (scheme process-context))
(import (scheme base))
(import (scheme read))
(import (scheme write))
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
