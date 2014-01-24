#! /bin/sh
#| -*- scheme -*-
exec csi -include-path /usr/local/share/scheme -s $0 "$@"
|#

(use r7rs)
;; (import-for-syntax r7rs)
(use srfi-69)
(require-library scheme.process-context)
(import (scheme process-context))
(include "snow/snowlib.sld")
(include "snow/srfi-60-integers-as-bits.sld")
(include "snow/bytevector.sld")
(include "snow/binio.sld")
(include "snow/bignum.sld")
(include "snow/random.sld")
(include "snow/filesys.sld")
(include "snow/digest.sld")
(include "snow/genport.sld")
(include "snow/zlib.sld")
(include "snow/tar.sld")
(include "seth/srfi-69-hash-tables.sld")
(include "seth/temporary-file.sld")
;; (include "seth/tar.sld")
(include "seth/http.sld")
(include "seth/snow2-utils.sld")
(include "seth/string-read-write.sld")
(import (chicken))
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
