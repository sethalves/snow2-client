#! /bin/sh
#| -*- scheme -*-
exec csi -include-path /usr/local/share/scheme -s $0 "$@"
|#

(use r7rs)
;; (import-for-syntax r7rs)
;; (use http-client)
(use srfi-69)
(require-library scheme.process-context)
(import (scheme process-context))
(include "seth/temporary-file.sld")
(include "seth/tar.sld")
(include "seth/http.sld")
(include "seth/snow2-utils.sld")
(include "seth/string-read-write.sld")
(import (chicken))
;; (import (prefix (seth http) http-))
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
