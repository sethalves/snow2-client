#! /bin/sh
#| -*- scheme -*-
exec chibi-scheme -I /usr/local/share/scheme -s $0 "$@"
|#

(import (scheme process-context))
(import (scheme base))
(import (scheme read))
(import (scheme write))
(import (prefix (seth snow2-utils) snow2-))
(import (seth string-read-write))

(define (usage pargs)
  (display (car pargs))
  (display " ")
  (display "<operation> '(library name)'")
  (newline)
  (display "  <operation> can be \"install\" or \"uninstall\"")
  (newline))


(define (main-program)
  (let ((repository-urls
         '("http://snow2.s3-website-us-east-1.amazonaws.com/"
           "http://snow-repository.s3-website-us-east-1.amazonaws.com/"))
        (pargs (command-line)))
    (cond ((not (= (length pargs) 3))
           (usage pargs))
          (else
           (let ((operation (list-ref pargs 1))
                 (library-name (read-from-string (list-ref pargs 2))))
             (snow2-client repository-urls operation library-name))))))


(main-program)
